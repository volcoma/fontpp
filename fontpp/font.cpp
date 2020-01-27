
#include "font.h"
#include "freetype/freetype.h"
#include "stb/stb.h"

#include <chrono>
#include <iostream>
#include <future>
#define COL32_R_SHIFT 0
#define COL32_G_SHIFT 8
#define COL32_B_SHIFT 16
#define COL32_A_SHIFT 24
#define COL32_A_MASK 0xFF000000
#define COL32(R, G, B, A)                                                                                    \
	((uint32_t(A) << COL32_A_SHIFT) | (uint32_t(B) << COL32_B_SHIFT) | (uint32_t(G) << COL32_G_SHIFT) |      \
	 (uint32_t(R) << COL32_R_SHIFT))

namespace fnt
{


template <typename T>
T sq(T t)
{
	return t * t;
}

float sample(const uint8_t* input, int width, int height, int x, int y, int spread)
{
	const auto lim = float(spread + 1);
	const auto lim_sq = sq(lim);
	const float dist_lim = std::sqrt(2 * lim_sq);

	float min_dist_to_pos = dist_lim;
	float min_dist_to_neg = dist_lim;

	const float inv255 = 1.0f / 255.0f;

	int32_t startx = x - spread;
	if(startx < 0)
		startx = 0;
	int32_t endx = x + spread + 1;
	if(endx > width)
		endx = width;

	int32_t starty = y - spread;
	if(starty < 0)
		starty = 0;
	int32_t endy = y + spread + 1;
	if(endy > height)
		endy = height;

	for(int32_t cy = starty, offsety = starty - y; cy < endy; ++cy, ++offsety)
	{
		int offset = cy * width;
		int offsety_sq = sq(offsety);

		for(int32_t cx = startx, offsetx = startx - x; cx < endx; ++cx, ++offsetx)
		{
			int i = offset + cx;
			uint8_t sample_int = input[i];

			// (sample-0.5) to support anti-aliased input
			float sample = float(sample_int) * inv255 - 0.5f;

			// eventually move to precalculated sqrt table?
			float dist = std::sqrt(float(sq(offsetx) + offsety_sq));

			if(sample_int > 0)
			{
				min_dist_to_pos = std::min(min_dist_to_pos, dist - sample);
			}
			if(sample_int < 255)
			{
				min_dist_to_neg = std::min(min_dist_to_neg, dist + sample);
			}
		}
	}

	assert(min_dist_to_pos >= 0 || min_dist_to_neg >= 0); // bad input

	const float inv_lim_half = 0.5f / lim;
	if(min_dist_to_pos > min_dist_to_neg)
	{
		return (0.5f - min_dist_to_pos * inv_lim_half);
	}
	return (0.5f + min_dist_to_neg * inv_lim_half);
}

// assumes input and output are big enough to NOT check bounds
// input can be anti-aliased
void generate_sdf(uint8_t* output, const uint8_t* input, int width, int height, int spread)
{
	for(int y = 0; y < height; ++y)
	{
		for(int x = 0; x < width; ++x)
		{
			int i = y * width + x;
			auto value = sample(input, width, height, x, y, spread);
			output[i] = uint8_t(std::round(clamp(value, 0.f, 1.f) * 255));
		}
	}
}

void generate_sdf_parallel(uint8_t* output, const uint8_t* input, int width, int height, int spread)
{
    parallel::parallel_for_2d(width, height, [&](int x, int y)
    {
        int i = y * width + x;

        auto value = sample(input, width, height, x, y, spread);
        output[i] = uint8_t(std::round(clamp(value, 0.f, 1.f) * 255));
    }, width / 4);
}

// Load file content into memory
// Memory allocated with std::malloc(), must be freed by user using std::free()
void* file_load_to_memory(const char* filename, const char* file_open_mode, size_t* out_file_size,
						  size_t padding_bytes)
{
	assert(filename && file_open_mode);
	if(out_file_size)
		*out_file_size = 0;

	FILE* f;
	if((f = fopen(filename, file_open_mode)) == nullptr)
		return nullptr;

	long file_size_signed;
	if(fseek(f, 0, SEEK_END) || (file_size_signed = ftell(f)) == -1 || fseek(f, 0, SEEK_SET))
	{
		fclose(f);
		return nullptr;
	}

	auto file_size = static_cast<size_t>(file_size_signed);
	void* file_data = std::malloc(file_size + padding_bytes);
	if(file_data == nullptr)
	{
		fclose(f);
		return nullptr;
	}
	if(fread(file_data, 1, file_size, f) != file_size)
	{
		fclose(f);
		std::free(file_data);
		return nullptr;
	}
	if(padding_bytes > 0)
		memset((reinterpret_cast<uint8_t*>(file_data) + file_size), 0, padding_bytes);

	fclose(f);
	if(out_file_size)
		*out_file_size = file_size;

	return file_data;
}

//-----------------------------------------------------------------------------
// [SECTION] MISC HELPERS/UTILITIES (ImText* functions)
//-----------------------------------------------------------------------------

// Convert UTF-8 to 32-bits character, process single character input.
// Based on stb_from_utf8() from github.com/nothings/stb/
// We handle UTF-8 decoding error by skipping forward.
int text_char_from_utf8(unsigned int* out_char, const char* in_text, const char* in_text_end)
{
	auto c = static_cast<unsigned int>(-1);
	auto str = reinterpret_cast<const uint8_t*>(in_text);
	if(!(*str & 0x80))
	{
		c = static_cast<unsigned int>(*str++);
		*out_char = c;
		return 1;
	}
	if((*str & 0xe0) == 0xc0)
	{
		*out_char = 0xFFFD; // will be invalid but not end of string
		if(in_text_end && in_text_end - reinterpret_cast<const char*>(str) < 2)
			return 1;
		if(*str < 0xc2)
			return 2;
		c = static_cast<unsigned int>((*str++ & 0x1f) << 6);
		if((*str & 0xc0) != 0x80)
			return 2;
		c += (*str++ & 0x3f);
		*out_char = c;
		return 2;
	}
	if((*str & 0xf0) == 0xe0)
	{
		*out_char = 0xFFFD; // will be invalid but not end of string
		if(in_text_end && in_text_end - reinterpret_cast<const char*>(str) < 3)
			return 1;
		if(*str == 0xe0 && (str[1] < 0xa0 || str[1] > 0xbf))
			return 3;
		if(*str == 0xed && str[1] > 0x9f)
			return 3; // str[1] < 0x80 is checked below
		c = static_cast<unsigned int>((*str++ & 0x0f) << 12);
		if((*str & 0xc0) != 0x80)
			return 3;
		c += static_cast<unsigned int>((*str++ & 0x3f) << 6);
		if((*str & 0xc0) != 0x80)
			return 3;
		c += (*str++ & 0x3f);
		*out_char = c;
		return 3;
	}
	if((*str & 0xf8) == 0xf0)
	{
		*out_char = 0xFFFD; // will be invalid but not end of string
		if(in_text_end && in_text_end - reinterpret_cast<const char*>(str) < 4)
			return 1;
		if(*str > 0xf4)
			return 4;
		if(*str == 0xf0 && (str[1] < 0x90 || str[1] > 0xbf))
			return 4;
		if(*str == 0xf4 && str[1] > 0x8f)
			return 4; // str[1] < 0x80 is checked below
		c = static_cast<unsigned int>((*str++ & 0x07) << 18);
		if((*str & 0xc0) != 0x80)
			return 4;
		c += static_cast<unsigned int>((*str++ & 0x3f) << 12);
		if((*str & 0xc0) != 0x80)
			return 4;
		c += static_cast<unsigned int>((*str++ & 0x3f) << 6);
		if((*str & 0xc0) != 0x80)
			return 4;
		c += (*str++ & 0x3f);
		// utf-8 encodings of values used in surrogate pairs are invalid
		if((c & 0xFFFFF800) == 0xD800)
			return 4;
		*out_char = c;
		return 4;
	}
	*out_char = 0;
	return 0;
}

template <typename T, std::size_t N>
constexpr std::size_t countof(T const (&)[N]) noexcept
{
	return N;
}

const unsigned int IM_TABSIZE = 4;

//-----------------------------------------------------------------------------
// [SECTION] font_atlas
//-----------------------------------------------------------------------------

font_atlas::~font_atlas()
{
	clear();
}

void font_atlas::clear_input_data()
{
	for(auto& i : config_data)
		if(i.font_data && i.font_data_owned_by_atlas)
		{
			std::free(i.font_data);
			i.font_data = nullptr;
		}

	// When clearing this we lose access to the font name and other information used to build the font.
	for(auto& font : fonts)
		if(font->config_data >= config_data.data() &&
		   font->config_data < config_data.data() + config_data.size())
		{
			font->config_data = nullptr;
			font->config_data_count = 0;
		}
	config_data.clear();
}

void font_atlas::clear_tex_data()
{
	std::vector<uint8_t>().swap(tex_pixels_alpha8);
	std::vector<uint32_t>().swap(tex_pixels_rgba32);
}

void font_atlas::clear_fonts()
{
	fonts.clear();
}

void font_atlas::clear()
{
	clear_input_data();
	clear_tex_data();
	clear_fonts();
}

void font_atlas::get_tex_data_as_alpha8(uint8_t** out_pixels, uint32_t* out_width, uint32_t* out_height,
										uint32_t* out_bytes_per_pixel)
{
	if(tex_pixels_alpha8.empty())
	{
		return;
	}

	*out_pixels = tex_pixels_alpha8.data();
	if(out_width)
		*out_width = tex_width;
	if(out_height)
		*out_height = tex_height;
	if(out_bytes_per_pixel)
		*out_bytes_per_pixel = 1;
}

void font_atlas::get_tex_data_as_rgba32(uint8_t** out_pixels, uint32_t* out_width, uint32_t* out_height,
										uint32_t* out_bytes_per_pixel)
{
	// Convert to RGBA32 format on demand
	// Although it is likely to be the most commonly used format, our font rendering is 1 channel / 8 bpp
	if(tex_pixels_rgba32.empty())
	{
		uint8_t* pixels = nullptr;
		get_tex_data_as_alpha8(&pixels, nullptr, nullptr);
		if(pixels)
		{
			tex_pixels_rgba32.resize(tex_width * tex_height, 0);
			const uint8_t* src = pixels;
			uint32_t* dst = tex_pixels_rgba32.data();
			for(uint32_t n = tex_width * tex_height; n > 0; n--)
				*dst++ = COL32(255, 255, 255, uint32_t(*src++));
		}
	}

	*out_pixels = reinterpret_cast<uint8_t*>(tex_pixels_rgba32.data());
	if(out_width)
		*out_width = tex_width;
	if(out_height)
		*out_height = tex_height;
	if(out_bytes_per_pixel)
		*out_bytes_per_pixel = 4;
}

bool font_atlas::build(font_rasterizer raster, std::string& err)
{
	if(sdf_spread > 0)
	{
		tex_glyph_padding += sdf_spread + 3;
	}

	switch(raster)
	{
		case font_rasterizer::freetype:
			return freetype::build(this, err);
		case font_rasterizer::stb:
			return stb::build(this, err);
		default:
			err = "unsupported font rasterizer";
			break;
	}

	return is_built();
}

void font_atlas::finish()
{
	// Build all fonts lookup tables
	for(auto& font : fonts)
		if(font->dirty_lookup_tables)
			font->build_lookup_table();

	if(sdf_spread > 0)
	{
		std::vector<uint8_t> sdf(tex_pixels_alpha8.size());

//        {
//            auto start = std::chrono::high_resolution_clock::now();
//            generate_sdf(sdf.data(), tex_pixels_alpha8.data(), int(tex_width), int(tex_height), int(sdf_spread));
//            auto end = std::chrono::high_resolution_clock::now();
//            auto dur = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);
//            std::cout << "[  serial  ] sdf vectorization on (" << tex_width << "x" << tex_height << ") took : " << dur.count() << "ms" << std::endl;
//        }
        {
            auto start = std::chrono::high_resolution_clock::now();
            generate_sdf_parallel(sdf.data(), tex_pixels_alpha8.data(), int(tex_width), int(tex_height), int(sdf_spread));
            auto end = std::chrono::high_resolution_clock::now();
            auto dur = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);
            std::cout << "[ parallel ] sdf vectorization on (" << tex_width << "x" << tex_height << ") took : " << dur.count() << "ms" << std::endl;
        }

        tex_pixels_alpha8 = std::move(sdf);
	}
}

void font_atlas::setup_font(font_info* font, font_config* font_config,
                            float ascent,
                            float descent,
                            float line_height,
                            float x_height,
                            float cap_height,
                            float ysuperscript_size,
                            float ysuperscript_offset,
                            float ysubscript_size,
                            float ysubscript_offset)
{
	if(!font_config->merge_mode)
	{
		font->clear_output_data();
		font->font_size = font_config->size_pixels;
		font->config_data = font_config;
		font->container_atlas = this;
		font->ascent = ascent;
		font->descent = descent;
		font->line_height = line_height;
        font->ysuperscript_size = ysuperscript_size;
        font->ysuperscript_offset = ysuperscript_offset;
        font->ysubscript_size = ysubscript_size;
        font->ysubscript_offset = ysubscript_offset;
        font->x_height = x_height;
        font->cap_height = cap_height;
	}
	font->config_data_count++;
}

font_info* font_atlas::add_font(const font_config* font_cfg)
{
	assert(font_cfg->font_data != nullptr && font_cfg->font_data_size > 0);
	assert(font_cfg->size_pixels > 0.0f);

	// Create new font
	if(!font_cfg->merge_mode)
		fonts.emplace_back(std::make_unique<font_info>());
	else
		assert(!fonts.empty() &&
			   "Cannot use MergeMode for the first font"); // When using MergeMode make sure that a font
														   // has already been added before. You can use
														   // atlas->add_font_default() to add
														   // the default font.

	config_data.push_back(*font_cfg);
	font_config& new_font_cfg = config_data.back();
	if(new_font_cfg.dst_font == nullptr)
		new_font_cfg.dst_font = fonts.back().get();
	if(!new_font_cfg.font_data_owned_by_atlas)
	{
		new_font_cfg.font_data = std::malloc(new_font_cfg.font_data_size);
		new_font_cfg.font_data_owned_by_atlas = true;
		if(new_font_cfg.font_data && font_cfg->font_data)
		{
			std::memcpy(new_font_cfg.font_data, font_cfg->font_data, new_font_cfg.font_data_size);
		}
	}

	// Invalidate texture
	clear_tex_data();
	return new_font_cfg.dst_font;
}

// Default font TTF is compressed with stb_compress then base85 encoded (see
// misc/fonts/binary_to_compressed_c.cpp for encoder)
static unsigned int stb_decompress_length(const uint8_t* input);
static unsigned int stb_decompress(uint8_t* output, const uint8_t* input, unsigned int length);
static const char* get_default_compressed_font_data_ttf_base85();
static unsigned int decode85_byte(uint8_t c)
{
	return c >= '\\' ? static_cast<unsigned int>(c) - 36 : static_cast<unsigned int>(c) - 35;
}
static void decode85(const uint8_t* src, uint8_t* dst)
{
	while(*src)
	{
		unsigned int tmp =
			decode85_byte(src[0]) +
			85 * (decode85_byte(src[1]) +
				  85 * (decode85_byte(src[2]) + 85 * (decode85_byte(src[3]) + 85 * decode85_byte(src[4]))));
		dst[0] = ((tmp >> 0) & 0xFF);
		dst[1] = ((tmp >> 8) & 0xFF);
		dst[2] = ((tmp >> 16) & 0xFF);
		dst[3] = ((tmp >> 24) & 0xFF); // We can't assume little-endianness.
		src += 5;
		dst += 4;
	}
}

// Load embedded ProggyClean.ttf at size 13, disable oversampling
font_info* font_atlas::add_font_default(const font_config* font_cfg_template)
{
	font_config font_cfg = font_cfg_template ? *font_cfg_template : font_config();
	if(!font_cfg_template)
	{
		font_cfg.oversample_h = font_cfg.oversample_v = 1;
		font_cfg.pixel_snap_h = true;
	}
	if(font_cfg.size_pixels <= 0.0f)
		font_cfg.size_pixels = 13.0f * 1.0f;

	const char* ttf_compressed_base85 = get_default_compressed_font_data_ttf_base85();
	const font_wchar* glyph_ranges =
		font_cfg.glyph_ranges != nullptr ? font_cfg.glyph_ranges : get_glyph_ranges_default();
	font_info* font = add_font_from_memory_compressed_base85_ttf(ttf_compressed_base85, font_cfg.size_pixels,
																 &font_cfg, glyph_ranges);
	return font;
}

font_info* font_atlas::add_font_from_file_ttf(const char* filename, float size_pixels,
											  const font_config* font_cfg_template,
											  const font_wchar* glyph_ranges)
{
	size_t data_size = 0;
	void* data = file_load_to_memory(filename, "rb", &data_size, 0);
	if(!data)
	{
		return nullptr;
	}
	font_config font_cfg = font_cfg_template ? *font_cfg_template : font_config();
	return add_font_from_memory_ttf(data, data_size, size_pixels, &font_cfg, glyph_ranges);
}

// NB: Transfer ownership of 'ttf_data' to font_atlas, unless font_cfg_template->FontDataOwnedByAtlas ==
// false. Owned TTF buffer will be deleted after build().
font_info* font_atlas::add_font_from_memory_ttf(void* ttf_data, size_t ttf_size, float size_pixels,
												const font_config* font_cfg_template,
												const font_wchar* glyph_ranges)
{
	font_config font_cfg = font_cfg_template ? *font_cfg_template : font_config();
	assert(font_cfg.font_data == nullptr);
	font_cfg.font_data = ttf_data;
	font_cfg.font_data_size = ttf_size;
	font_cfg.size_pixels = size_pixels;
	if(glyph_ranges)
		font_cfg.glyph_ranges = glyph_ranges;
	return add_font(&font_cfg);
}

font_info* font_atlas::add_font_from_memory_compressed_ttf(const void* compressed_ttf_data,
														   size_t compressed_ttf_size, float size_pixels,
														   const font_config* font_cfg_template,
														   const font_wchar* glyph_ranges)
{
	const unsigned int buf_decompressed_size =
		stb_decompress_length(reinterpret_cast<const uint8_t*>(compressed_ttf_data));
	auto buf_decompressed_data = reinterpret_cast<uint8_t*>(std::malloc(buf_decompressed_size));
	stb_decompress(buf_decompressed_data, reinterpret_cast<const uint8_t*>(compressed_ttf_data),
				   static_cast<unsigned int>(compressed_ttf_size));

	font_config font_cfg = font_cfg_template ? *font_cfg_template : font_config();
	assert(font_cfg.font_data == nullptr);
	font_cfg.font_data_owned_by_atlas = true;
	return add_font_from_memory_ttf(buf_decompressed_data, size_t(buf_decompressed_size), size_pixels,
									&font_cfg, glyph_ranges);
}

font_info* font_atlas::add_font_from_memory_compressed_base85_ttf(const char* compressed_ttf_data_base85,
																  float size_pixels,
																  const font_config* font_cfg,
																  const font_wchar* glyph_ranges)
{
	size_t compressed_ttf_size = ((strlen(compressed_ttf_data_base85) + 4) / 5) * 4;
	void* compressed_ttf = std::malloc(compressed_ttf_size);
	decode85(reinterpret_cast<const uint8_t*>(compressed_ttf_data_base85),
			 reinterpret_cast<uint8_t*>(compressed_ttf));
	font_info* font = add_font_from_memory_compressed_ttf(compressed_ttf, compressed_ttf_size, size_pixels,
														  font_cfg, glyph_ranges);
	std::free(compressed_ttf);
	return font;
}

// Retrieve list of range (2 int per range, values are inclusive)
const font_wchar* get_glyph_ranges_default()
{
	return get_glyph_ranges_latin();
}

const font_wchar* get_glyph_ranges_latin()
{
	static const font_wchar ranges[] = {
		0x0020,
		0x00FF, // Basic Latin + Latin Supplement
		0,
	};
	return &ranges[0];
}

const font_wchar* get_glyph_ranges_korean()
{
	static const font_wchar ranges[] = {
		0x0020, 0x00FF, // Basic Latin + Latin Supplement
		0x3131, 0x3163, // Korean alphabets
		0xAC00, 0xD79D, // Korean characters
		0,
	};
	return &ranges[0];
}

const font_wchar* get_glyph_ranges_all()
{
	static const font_wchar ranges[] = {
		0x0000, 0xFFFF,
		0,
	};
	return &ranges[0];
}

const font_wchar* get_glyph_ranges_chinese_full()
{
	static const font_wchar ranges[] = {
		0x0020, 0x00FF, // Basic Latin + Latin Supplement
		0x2000, 0x206F, // General Punctuation
		0x3000, 0x30FF, // CJK Symbols and Punctuations, Hiragana, Katakana
		0x31F0, 0x31FF, // Katakana Phonetic Extensions
		0xFF00, 0xFFEF, // Half-width characters
		0x4e00, 0x9FAF, // CJK Ideograms
		0,
	};
	return &ranges[0];
}

static void unpack_accumulative_offsets_into_ranges(int base_codepoint, const short* accumulative_offsets,
													size_t accumulative_offsets_count, font_wchar* out_ranges)
{
	for(size_t n = 0; n < accumulative_offsets_count; n++, out_ranges += 2)
	{
		out_ranges[0] = out_ranges[1] = font_wchar(base_codepoint + accumulative_offsets[n]);
		base_codepoint += accumulative_offsets[n];
	}
	out_ranges[0] = 0;
}

//-------------------------------------------------------------------------
// [SECTION] font_atlas glyph ranges helpers
//-------------------------------------------------------------------------

const font_wchar* get_glyph_ranges_chinese_simplified_common()
{
	// Store 2500 regularly used characters for Simplified Chinese.
	// Sourced from
	// https://zh.wiktionary.org/wiki/%E9%99%84%E5%BD%95:%E7%8E%B0%E4%BB%A3%E6%B1%89%E8%AF%AD%E5%B8%B8%E7%94%A8%E5%AD%97%E8%A1%A8
	// This table covers 97.97% of all characters used during the month in July, 1987.
	// You can use font_glyph_ranges_builder to create your own ranges derived from this, by merging existing
	// ranges or adding new characters. (Stored as accumulative offsets from the initial unicode codepoint
	// 0x4E00. This encoding is designed to helps us compact the source code size.)
	static const short accumulative_offsets[] = {
		0,  1,  2,   4,  1,   1,   1,   1,   2,   1,   3,   2,   1,   2,   2,  1,   1,  1,  1,  1,  5,   2,
		1,  2,  3,   3,  3,   2,   2,   4,   1,   1,   1,   2,   1,   5,   2,  3,   1,  2,  1,  2,  1,   1,
		2,  1,  1,   2,  2,   1,   4,   1,   1,   1,   1,   5,   10,  1,   2,  19,  2,  1,  2,  1,  2,   1,
		2,  1,  2,   1,  5,   1,   6,   3,   2,   1,   2,   2,   1,   1,   1,  4,   8,  5,  1,  1,  4,   1,
		1,  3,  1,   2,  1,   5,   1,   2,   1,   1,   1,   10,  1,   1,   5,  2,   4,  6,  1,  4,  2,   2,
		2,  12, 2,   1,  1,   6,   1,   1,   1,   4,   1,   1,   4,   6,   5,  1,   4,  2,  2,  4,  10,  7,
		1,  1,  4,   2,  4,   2,   1,   4,   3,   6,   10,  12,  5,   7,   2,  14,  2,  9,  1,  1,  6,   7,
		10, 4,  7,   13, 1,   5,   4,   8,   4,   1,   1,   2,   28,  5,   6,  1,   1,  5,  2,  5,  20,  2,
		2,  9,  8,   11, 2,   9,   17,  1,   8,   6,   8,   27,  4,   6,   9,  20,  11, 27, 6,  68, 2,   2,
		1,  1,  1,   2,  1,   2,   2,   7,   6,   11,  3,   3,   1,   1,   3,  1,   2,  1,  1,  1,  1,   1,
		3,  1,  1,   8,  3,   4,   1,   5,   7,   2,   1,   4,   4,   8,   4,  2,   1,  2,  1,  1,  4,   5,
		6,  3,  6,   2,  12,  3,   1,   3,   9,   2,   4,   3,   4,   1,   5,  3,   3,  1,  3,  7,  1,   5,
		1,  1,  1,   1,  2,   3,   4,   5,   2,   3,   2,   6,   1,   1,   2,  1,   7,  1,  7,  3,  4,   5,
		15, 2,  2,   1,  5,   3,   22,  19,  2,   1,   1,   1,   1,   2,   5,  1,   1,  1,  6,  1,  1,   12,
		8,  2,  9,   18, 22,  4,   1,   1,   5,   1,   16,  1,   2,   7,   10, 15,  1,  1,  6,  2,  4,   1,
		2,  4,  1,   6,  1,   1,   3,   2,   4,   1,   6,   4,   5,   1,   2,  1,   1,  2,  1,  10, 3,   1,
		3,  2,  1,   9,  3,   2,   5,   7,   2,   19,  4,   3,   6,   1,   1,  1,   1,  1,  4,  3,  2,   1,
		1,  1,  2,   5,  3,   1,   1,   1,   2,   2,   1,   1,   2,   1,   1,  2,   1,  3,  1,  1,  1,   3,
		7,  1,  4,   1,  1,   2,   1,   1,   2,   1,   2,   4,   4,   3,   8,  1,   1,  1,  2,  1,  3,   5,
		1,  3,  1,   3,  4,   6,   2,   2,   14,  4,   6,   6,   11,  9,   1,  15,  3,  1,  28, 5,  2,   5,
		5,  3,  1,   3,  4,   5,   4,   6,   14,  3,   2,   3,   5,   21,  2,  7,   20, 10, 1,  2,  19,  2,
		4,  28, 28,  2,  3,   2,   1,   14,  4,   1,   26,  28,  42,  12,  40, 3,   52, 79, 5,  14, 17,  3,
		2,  2,  11,  3,  4,   6,   3,   1,   8,   2,   23,  4,   5,   8,   10, 4,   2,  7,  3,  5,  1,   1,
		6,  3,  1,   2,  2,   2,   5,   28,  1,   1,   7,   7,   20,  5,   3,  29,  3,  17, 26, 1,  8,   4,
		27, 3,  6,   11, 23,  5,   3,   4,   6,   13,  24,  16,  6,   5,   10, 25,  35, 7,  3,  2,  3,   3,
		14, 3,  6,   2,  6,   1,   4,   2,   3,   8,   2,   1,   1,   3,   3,  3,   4,  1,  1,  13, 2,   2,
		4,  5,  2,   1,  14,  14,  1,   2,   2,   1,   4,   5,   2,   3,   1,  14,  3,  12, 3,  17, 2,   16,
		5,  1,  2,   1,  8,   9,   3,   19,  4,   2,   2,   4,   17,  25,  21, 20,  28, 75, 1,  10, 29,  103,
		4,  1,  2,   1,  1,   4,   2,   4,   1,   2,   3,   24,  2,   2,   2,  1,   1,  2,  1,  3,  8,   1,
		1,  1,  2,   1,  1,   3,   1,   1,   1,   6,   1,   5,   3,   1,   1,  1,   3,  4,  1,  1,  5,   2,
		1,  5,  6,   13, 9,   16,  1,   1,   1,   1,   3,   2,   3,   2,   4,  5,   2,  5,  2,  2,  3,   7,
		13, 7,  2,   2,  1,   1,   1,   1,   2,   3,   3,   2,   1,   6,   4,  9,   2,  1,  14, 2,  14,  2,
		1,  18, 3,   4,  14,  4,   11,  41,  15,  23,  15,  23,  176, 1,   3,  4,   1,  1,  1,  1,  5,   3,
		1,  2,  3,   7,  3,   1,   1,   2,   1,   2,   4,   4,   6,   2,   4,  1,   9,  7,  1,  10, 5,   8,
		16, 29, 1,   1,  2,   2,   3,   1,   3,   5,   2,   4,   5,   4,   1,  1,   2,  2,  3,  3,  7,   1,
		6,  10, 1,   17, 1,   44,  4,   6,   2,   1,   1,   6,   5,   4,   2,  10,  1,  6,  9,  2,  8,   1,
		24, 1,  2,   13, 7,   8,   8,   2,   1,   4,   1,   3,   1,   3,   3,  5,   2,  5,  10, 9,  4,   9,
		12, 2,  1,   6,  1,   10,  1,   1,   7,   7,   4,   10,  8,   3,   1,  13,  4,  3,  1,  6,  1,   3,
		5,  2,  1,   2,  17,  16,  5,   2,   16,  6,   1,   4,   2,   1,   3,  3,   6,  8,  5,  11, 11,  1,
		3,  3,  2,   4,  6,   10,  9,   5,   7,   4,   7,   4,   7,   1,   1,  4,   2,  1,  3,  6,  8,   7,
		1,  6,  11,  5,  5,   3,   24,  9,   4,   2,   7,   13,  5,   1,   8,  82,  16, 61, 1,  1,  1,   4,
		2,  2,  16,  10, 3,   8,   1,   1,   6,   4,   2,   1,   3,   1,   1,  1,   4,  3,  8,  4,  2,   2,
		1,  1,  1,   1,  1,   6,   3,   5,   1,   1,   4,   6,   9,   2,   1,  1,   1,  2,  1,  7,  2,   1,
		6,  1,  5,   4,  4,   3,   1,   8,   1,   3,   3,   1,   3,   2,   2,  2,   2,  3,  1,  6,  1,   2,
		1,  2,  1,   3,  7,   1,   8,   2,   1,   2,   1,   5,   2,   5,   3,  5,   10, 1,  2,  1,  1,   3,
		2,  5,  11,  3,  9,   3,   5,   1,   1,   5,   9,   1,   2,   1,   5,  7,   9,  9,  8,  1,  3,   3,
		3,  6,  8,   2,  3,   2,   1,   1,   32,  6,   1,   2,   15,  9,   3,  7,   13, 1,  3,  10, 13,  2,
		14, 1,  13,  10, 2,   1,   3,   10,  4,   15,  2,   15,  15,  10,  1,  3,   9,  6,  9,  32, 25,  26,
		47, 7,  3,   2,  3,   1,   6,   3,   4,   3,   2,   8,   5,   4,   1,  9,   4,  2,  2,  19, 10,  6,
		2,  3,  8,   1,  2,   2,   4,   2,   1,   9,   4,   4,   4,   6,   4,  8,   9,  2,  3,  1,  1,   1,
		1,  3,  5,   5,  1,   3,   8,   4,   6,   2,   1,   4,   12,  1,   5,  3,   7,  13, 2,  5,  8,   1,
		6,  1,  2,   5,  14,  6,   1,   5,   2,   4,   8,   15,  5,   1,   23, 6,   62, 2,  10, 1,  1,   8,
		1,  2,  2,   10, 4,   2,   2,   9,   2,   1,   1,   3,   2,   3,   1,  5,   3,  3,  2,  1,  3,   8,
		1,  1,  1,   11, 3,   1,   1,   4,   3,   7,   1,   14,  1,   2,   3,  12,  5,  2,  5,  1,  6,   7,
		5,  7,  14,  11, 1,   3,   1,   8,   9,   12,  2,   1,   11,  8,   4,  4,   2,  6,  10, 9,  13,  1,
		1,  3,  1,   5,  1,   3,   2,   4,   4,   1,   18,  2,   3,   14,  11, 4,   29, 4,  2,  7,  1,   3,
		13, 9,  2,   2,  5,   3,   5,   20,  7,   16,  8,   5,   72,  34,  6,  4,   22, 12, 12, 28, 45,  36,
		9,  7,  39,  9,  191, 1,   1,   1,   4,   11,  8,   4,   9,   2,   3,  22,  1,  1,  1,  1,  4,   17,
		1,  7,  7,   1,  11,  31,  10,  2,   4,   8,   2,   3,   2,   1,   4,  2,   16, 4,  32, 2,  3,   19,
		13, 4,  9,   1,  5,   2,   14,  8,   1,   1,   3,   6,   19,  6,   5,  1,   16, 6,  2,  10, 8,   5,
		1,  2,  3,   1,  5,   5,   1,   11,  6,   6,   1,   3,   3,   2,   6,  3,   8,  1,  1,  4,  10,  7,
		5,  7,  7,   5,  8,   9,   2,   1,   3,   4,   1,   1,   3,   1,   3,  3,   2,  6,  16, 1,  4,   6,
		3,  1,  10,  6,  1,   3,   15,  2,   9,   2,   10,  25,  13,  9,   16, 6,   2,  2,  10, 11, 4,   3,
		9,  1,  2,   6,  6,   5,   4,   30,  40,  1,   10,  7,   12,  14,  33, 6,   3,  6,  7,  3,  1,   3,
		1,  11, 14,  4,  9,   5,   12,  11,  49,  18,  51,  31,  140, 31,  2,  2,   1,  5,  1,  8,  1,   10,
		1,  4,  4,   3,  24,  1,   10,  1,   3,   6,   6,   16,  3,   4,   5,  2,   1,  4,  2,  57, 10,  6,
		22, 2,  22,  3,  7,   22,  6,   10,  11,  36,  18,  16,  33,  36,  2,  5,   5,  1,  1,  1,  4,   10,
		1,  4,  13,  2,  7,   5,   2,   9,   3,   4,   1,   7,   43,  3,   7,  3,   9,  14, 7,  9,  1,   11,
		1,  1,  3,   7,  4,   18,  13,  1,   14,  1,   3,   6,   10,  73,  2,  2,   30, 6,  1,  11, 18,  19,
		13, 22, 3,   46, 42,  37,  89,  7,   3,   16,  34,  2,   2,   3,   9,  1,   7,  1,  1,  1,  2,   2,
		4,  10, 7,   3,  10,  3,   9,   5,   28,  9,   2,   6,   13,  7,   3,  1,   3,  10, 2,  7,  2,   11,
		3,  6,  21,  54, 85,  2,   1,   4,   2,   2,   1,   39,  3,   21,  2,  2,   5,  1,  1,  1,  4,   1,
		1,  3,  4,   15, 1,   3,   2,   4,   4,   2,   3,   8,   2,   20,  1,  8,   7,  13, 4,  1,  26,  6,
		2,  9,  34,  4,  21,  52,  10,  4,   4,   1,   5,   12,  2,   11,  1,  7,   2,  30, 12, 44, 2,   30,
		1,  1,  3,   6,  16,  9,   17,  39,  82,  2,   2,   24,  7,   1,   7,  3,   16, 9,  14, 44, 2,   1,
		2,  1,  2,   3,  5,   2,   4,   1,   6,   7,   5,   3,   2,   6,   1,  11,  5,  11, 2,  1,  18,  19,
		8,  1,  3,   24, 29,  2,   1,   3,   5,   2,   2,   1,   13,  6,   5,  1,   46, 11, 3,  5,  1,   1,
		5,  8,  2,   10, 6,   12,  6,   3,   7,   11,  2,   4,   16,  13,  2,  5,   1,  1,  2,  2,  5,   2,
		28, 5,  2,   23, 10,  8,   4,   4,   22,  39,  95,  38,  8,   14,  9,  5,   1,  13, 5,  4,  3,   13,
		12, 11, 1,   9,  1,   27,  37,  2,   5,   4,   4,   63,  211, 95,  2,  2,   2,  1,  3,  5,  2,   1,
		1,  2,  2,   1,  1,   1,   3,   2,   4,   1,   2,   1,   1,   5,   2,  2,   1,  1,  2,  3,  1,   3,
		1,  1,  1,   3,  1,   4,   2,   1,   3,   6,   1,   1,   3,   7,   15, 5,   3,  2,  5,  3,  9,   11,
		4,  2,  22,  1,  6,   3,   8,   7,   1,   4,   28,  4,   16,  3,   3,  25,  4,  4,  27, 27, 1,   4,
		1,  2,  2,   7,  1,   3,   5,   2,   28,  8,   2,   14,  1,   8,   6,  16,  25, 3,  3,  3,  14,  3,
		3,  1,  1,   2,  1,   4,   6,   3,   8,   4,   1,   1,   1,   2,   3,  6,   10, 6,  2,  3,  18,  3,
		2,  5,  5,   4,  3,   1,   5,   2,   5,   4,   23,  7,   6,   12,  6,  4,   17, 11, 9,  5,  1,   1,
		10, 5,  12,  1,  1,   11,  26,  33,  7,   3,   6,   1,   17,  7,   1,  5,   12, 1,  11, 2,  4,   1,
		8,  14, 17,  23, 1,   2,   1,   7,   8,   16,  11,  9,   6,   5,   2,  6,   4,  16, 2,  8,  14,  1,
		11, 8,  9,   1,  1,   1,   9,   25,  4,   11,  19,  7,   2,   15,  2,  12,  8,  52, 7,  5,  19,  2,
		16, 4,  36,  8,  1,   16,  8,   24,  26,  4,   6,   2,   9,   5,   4,  36,  3,  28, 12, 25, 15,  37,
		27, 17, 12,  59, 38,  5,   32,  127, 1,   2,   9,   17,  14,  4,   1,  2,   1,  1,  8,  11, 50,  4,
		14, 2,  19,  16, 4,   17,  5,   4,   5,   26,  12,  45,  2,   23,  45, 104, 30, 12, 8,  3,  10,  2,
		2,  3,  3,   1,  4,   20,  7,   2,   9,   6,   15,  2,   20,  1,   3,  16,  4,  11, 15, 6,  134, 2,
		5,  59, 1,   2,  2,   2,   1,   9,   17,  3,   26,  137, 10,  211, 59, 1,   2,  4,  1,  4,  1,   1,
		1,  2,  6,   2,  3,   1,   1,   2,   3,   2,   3,   1,   3,   4,   4,  2,   3,  3,  1,  4,  3,   1,
		7,  2,  2,   3,  1,   2,   1,   3,   3,   3,   2,   2,   3,   2,   1,  3,   14, 6,  1,  3,  2,   9,
		6,  15, 27,  9,  34,  145, 1,   1,   2,   1,   1,   1,   1,   2,   1,  1,   1,  1,  2,  2,  2,   3,
		1,  2,  1,   1,  1,   2,   3,   5,   8,   3,   5,   2,   4,   1,   3,  2,   2,  2,  12, 4,  1,   1,
		1,  10, 4,   5,  1,   20,  4,   16,  1,   15,  9,   5,   12,  2,   9,  2,   5,  4,  2,  26, 19,  7,
		1,  26, 4,   30, 12,  15,  42,  1,   6,   8,   172, 1,   1,   4,   2,  1,   1,  11, 2,  2,  4,   2,
		1,  2,  1,   10, 8,   1,   2,   1,   4,   5,   1,   2,   5,   1,   8,  4,   1,  3,  4,  2,  1,   6,
		2,  1,  3,   4,  1,   2,   1,   1,   1,   1,   12,  5,   7,   2,   4,  3,   1,  1,  1,  3,  3,   6,
		1,  2,  2,   3,  3,   3,   2,   1,   2,   12,  14,  11,  6,   6,   4,  12,  2,  8,  1,  7,  10,  1,
		35, 7,  4,   13, 15,  4,   3,   23,  21,  28,  52,  5,   26,  5,   6,  1,   7,  10, 2,  7,  53,  3,
		2,  1,  1,   1,  2,   163, 532, 1,   10,  11,  1,   3,   3,   4,   8,  2,   8,  6,  2,  2,  23,  22,
		4,  2,  2,   4,  2,   1,   3,   1,   3,   3,   5,   9,   8,   2,   1,  2,   8,  1,  10, 2,  12,  21,
		20, 15, 105, 2,  3,   1,   1,   3,   2,   3,   1,   1,   2,   5,   1,  4,   15, 11, 19, 1,  1,   1,
		1,  5,  4,   5,  1,   1,   2,   5,   3,   5,   12,  1,   2,   5,   1,  11,  1,  1,  15, 9,  1,   4,
		5,  3,  26,  8,  2,   1,   3,   1,   1,   15,  19,  2,   12,  1,   2,  5,   2,  7,  2,  19, 2,   20,
		6,  26, 7,   5,  2,   2,   7,   34,  21,  13,  70,  2,   128, 1,   1,  2,   1,  1,  2,  1,  1,   3,
		2,  2,  2,   15, 1,   4,   1,   3,   4,   42,  10,  6,   1,   49,  85, 8,   1,  2,  1,  1,  4,   4,
		2,  3,  6,   1,  5,   7,   4,   3,   211, 4,   1,   2,   1,   2,   5,  1,   2,  4,  2,  2,  6,   5,
		6,  10, 3,   4,  48,  100, 6,   2,   16,  296, 5,   27,  387, 2,   2,  3,   7,  16, 8,  5,  38,  15,
		39, 21, 9,   10, 3,   7,   59,  13,  27,  21,  47,  5,   21,  6};
	static font_wchar base_ranges[] = // not zero-terminated
		{
			0x0020, 0x00FF, // Basic Latin + Latin Supplement
			0x2000, 0x206F, // General Punctuation
			0x3000, 0x30FF, // CJK Symbols and Punctuations, Hiragana, Katakana
			0x31F0, 0x31FF, // Katakana Phonetic Extensions
			0xFF00, 0xFFEF  // Half-width characters
		};
	static font_wchar full_ranges[countof(base_ranges) + countof(accumulative_offsets) * 2 + 1] = {0};
	if(!full_ranges[0])
	{
		memcpy(full_ranges, base_ranges, sizeof(base_ranges));
		unpack_accumulative_offsets_into_ranges(0x4E00, accumulative_offsets, countof(accumulative_offsets),
												full_ranges + countof(base_ranges));
	}
	return &full_ranges[0];
}

const font_wchar* get_glyph_ranges_japanese()
{
	// 1946 common ideograms code points for Japanese
	// Sourced from http://theinstructionlimit.com/common-kanji-character-ranges-for-xna-spritefont-rendering
	// FIXME: Source a list of the revised 2136 Joyo Kanji list from 2010 and rebuild this.
	// You can use font_glyph_ranges_builder to create your own ranges derived from this, by merging existing
	// ranges or adding new characters. (Stored as accumulative offsets from the initial unicode codepoint
	// 0x4E00. This encoding is designed to helps us compact the source code size.)
	static const short accumulative_offsets[] = {
		0,  1,  2,   4,  1,   1,   1,  1,  2,  1,  6,   2,   2,  1,   8,  5,   7,   11,  1,  2,   10, 10,  8,
		2,  4,  20,  2,  11,  8,   2,  1,  2,  1,  6,   2,   1,  7,   5,  3,   7,   1,   1,  13,  7,  9,   1,
		4,  6,  1,   2,  1,   10,  1,  1,  9,  2,  2,   4,   5,  6,   14, 1,   1,   9,   3,  18,  5,  4,   2,
		2,  10, 7,   1,  1,   1,   3,  2,  4,  3,  23,  2,   10, 12,  2,  14,  2,   4,   13, 1,   6,  10,  3,
		1,  7,  13,  6,  4,   13,  5,  2,  3,  17, 2,   2,   5,  7,   6,  4,   1,   7,   14, 16,  6,  13,  9,
		15, 1,  1,   7,  16,  4,   7,  1,  19, 9,  2,   7,   15, 2,   6,  5,   13,  25,  4,  14,  13, 11,  25,
		1,  1,  1,   2,  1,   2,   2,  3,  10, 11, 3,   3,   1,  1,   4,  4,   2,   1,   4,  9,   1,  4,   3,
		5,  5,  2,   7,  12,  11,  15, 7,  16, 4,  5,   16,  2,  1,   1,  6,   3,   3,   1,  1,   2,  7,   6,
		6,  7,  1,   4,  7,   6,   1,  1,  2,  1,  12,  3,   3,  9,   5,  8,   1,   11,  1,  2,   3,  18,  20,
		4,  1,  3,   6,  1,   7,   3,  5,  5,  7,  2,   2,   12, 3,   1,  4,   2,   3,   2,  3,   11, 8,   7,
		4,  17, 1,   9,  25,  1,   1,  4,  2,  2,  4,   1,   2,  7,   1,  1,   1,   3,   1,  2,   6,  16,  1,
		2,  1,  1,   3,  12,  20,  2,  5,  20, 8,  7,   6,   2,  1,   1,  1,   1,   6,   2,  1,   2,  10,  1,
		1,  6,  1,   3,  1,   2,   1,  4,  1,  12, 4,   1,   3,  1,   1,  1,   1,   1,   10, 4,   7,  5,   13,
		1,  15, 1,   1,  30,  11,  9,  1,  15, 38, 14,  1,   32, 17,  20, 1,   9,   31,  2,  21,  9,  4,   49,
		22, 2,  1,   13, 1,   11,  45, 35, 43, 55, 12,  19,  83, 1,   3,  2,   3,   13,  2,  1,   7,  3,   18,
		3,  13, 8,   1,  8,   18,  5,  3,  7,  25, 24,  9,   24, 40,  3,  17,  24,  2,   1,  6,   2,  3,   16,
		15, 6,  7,   3,  12,  1,   9,  7,  3,  3,  3,   15,  21, 5,   16, 4,   5,   12,  11, 11,  3,  6,   3,
		2,  31, 3,   2,  1,   1,   23, 6,  6,  1,  4,   2,   6,  5,   2,  1,   1,   3,   3,  22,  2,  6,   2,
		3,  17, 3,   2,  4,   5,   1,  9,  5,  1,  1,   6,   15, 12,  3,  17,  2,   14,  2,  8,   1,  23,  16,
		4,  2,  23,  8,  15,  23,  20, 12, 25, 19, 47,  11,  21, 65,  46, 4,   3,   1,   5,  6,   1,  2,   5,
		26, 2,  1,   1,  3,   11,  1,  1,  1,  2,  1,   2,   3,  1,   1,  10,  2,   3,   1,  1,   1,  3,   6,
		3,  2,  2,   6,  6,   9,   2,  2,  2,  6,  2,   5,   10, 2,   4,  1,   2,   1,   2,  2,   3,  1,   1,
		3,  1,  2,   9,  23,  9,   2,  1,  1,  1,  1,   5,   3,  2,   1,  10,  9,   6,   1,  10,  2,  31,  25,
		3,  7,  5,   40, 1,   15,  6,  17, 7,  27, 180, 1,   3,  2,   2,  1,   1,   1,   6,  3,   10, 7,   1,
		3,  6,  17,  8,  6,   2,   2,  1,  3,  5,  5,   8,   16, 14,  15, 1,   1,   4,   1,  2,   1,  1,   1,
		3,  2,  7,   5,  6,   2,   5,  10, 1,  4,  2,   9,   1,  1,   11, 6,   1,   44,  1,  3,   7,  9,   5,
		1,  3,  1,   1,  10,  7,   1,  10, 4,  2,  7,   21,  15, 7,   2,  5,   1,   8,   3,  4,   1,  3,   1,
		6,  1,  4,   2,  1,   4,   10, 8,  1,  4,  5,   1,   5,  10,  2,  7,   1,   10,  1,  1,   3,  4,   11,
		10, 29, 4,   7,  3,   5,   2,  3,  33, 5,  2,   19,  3,  1,   4,  2,   6,   31,  11, 1,   3,  3,   3,
		1,  8,  10,  9,  12,  11,  12, 8,  3,  14, 8,   6,   11, 1,   4,  41,  3,   1,   2,  7,   13, 1,   5,
		6,  2,  6,   12, 12,  22,  5,  9,  4,  8,  9,   9,   34, 6,   24, 1,   1,   20,  9,  9,   3,  4,   1,
		7,  2,  2,   2,  6,   2,   28, 5,  3,  6,  1,   4,   6,  7,   4,  2,   1,   4,   2,  13,  6,  4,   4,
		3,  1,  8,   8,  3,   2,   1,  5,  1,  2,  2,   3,   1,  11,  11, 7,   3,   6,   10, 8,   6,  16,  16,
		22, 7,  12,  6,  21,  5,   4,  6,  6,  3,  6,   1,   3,  2,   1,  2,   8,   29,  1,  10,  1,  6,   13,
		6,  6,  19,  31, 1,   13,  4,  4,  22, 17, 26,  33,  10, 4,   15, 12,  25,  6,   67, 10,  2,  3,   1,
		6,  10, 2,   6,  2,   9,   1,  9,  4,  4,  1,   2,   16, 2,   5,  9,   2,   3,   8,  1,   8,  3,   9,
		4,  8,  6,   4,  8,   11,  3,  2,  1,  1,  3,   26,  1,  7,   5,  1,   11,  1,   5,  3,   5,  2,   13,
		6,  39, 5,   1,  5,   2,   11, 6,  10, 5,  1,   15,  5,  3,   6,  19,  21,  22,  2,  4,   1,  6,   1,
		8,  1,  4,   8,  2,   4,   2,  2,  9,  2,  1,   1,   1,  4,   3,  6,   3,   12,  7,  1,   14, 2,   4,
		10, 2,  13,  1,  17,  7,   3,  2,  1,  3,  2,   13,  7,  14,  12, 3,   1,   29,  2,  8,   9,  15,  14,
		9,  14, 1,   3,  1,   6,   5,  9,  11, 3,  38,  43,  20, 7,   7,  8,   5,   15,  12, 19,  15, 81,  8,
		7,  1,  5,   73, 13,  37,  28, 8,  8,  1,  15,  18,  20, 165, 28, 1,   6,   11,  8,  4,   14, 7,   15,
		1,  3,  3,   6,  4,   1,   7,  14, 1,  1,  11,  30,  1,  5,   1,  4,   14,  1,   4,  2,   7,  52,  2,
		6,  29, 3,   1,  9,   1,   21, 3,  5,  1,  26,  3,   11, 14,  11, 1,   17,  5,   1,  2,   1,  3,   2,
		8,  1,  2,   9,  12,  1,   1,  2,  3,  8,  3,   24,  12, 7,   7,  5,   17,  3,   3,  3,   1,  23,  10,
		4,  4,  6,   3,  1,   16,  17, 22, 3,  10, 21,  16,  16, 6,   4,  10,  2,   1,   1,  2,   8,  8,   6,
		5,  3,  3,   3,  39,  25,  15, 1,  1,  16, 6,   7,   25, 15,  6,  6,   12,  1,   22, 13,  1,  4,   9,
		5,  12, 2,   9,  1,   12,  28, 8,  3,  5,  10,  22,  60, 1,   2,  40,  4,   61,  63, 4,   1,  13,  12,
		1,  4,  31,  12, 1,   14,  89, 5,  16, 6,  29,  14,  2,  5,   49, 18,  18,  5,   29, 33,  47, 1,   17,
		1,  19, 12,  2,  9,   7,   39, 12, 3,  7,  12,  39,  3,  1,   46, 4,   12,  3,   8,  9,   5,  31,  15,
		18, 3,  2,   2,  66,  19,  13, 17, 5,  3,  46,  124, 13, 57,  34, 2,   5,   4,   5,  8,   1,  1,   1,
		4,  3,  1,   17, 5,   3,   5,  3,  1,  8,  5,   6,   3,  27,  3,  26,  7,   12,  7,  2,   17, 3,   7,
		18, 78, 16,  4,  36,  1,   2,  1,  6,  2,  1,   39,  17, 7,   4,  13,  4,   4,   4,  1,   10, 4,   2,
		4,  6,  3,   10, 1,   19,  1,  26, 2,  4,  33,  2,   73, 47,  7,  3,   8,   2,   4,  15,  18, 1,   29,
		2,  41, 14,  1,  21,  16,  41, 7,  39, 25, 13,  44,  2,  2,   10, 1,   13,  7,   1,  7,   3,  5,   20,
		4,  8,  2,   49, 1,   10,  6,  1,  6,  7,  10,  7,   11, 16,  3,  12,  20,  4,   10, 3,   1,  2,   11,
		2,  28, 9,   2,  4,   7,   2,  15, 1,  27, 1,   28,  17, 4,   5,  10,  7,   3,   24, 10,  11, 6,   26,
		3,  2,  7,   2,  2,   49,  16, 10, 16, 15, 4,   5,   27, 61,  30, 14,  38,  22,  2,  7,   5,  1,   3,
		12, 23, 24,  17, 17,  3,   3,  2,  4,  1,  6,   2,   7,  5,   1,  1,   5,   1,   1,  9,   4,  1,   3,
		6,  1,  8,   2,  8,   4,   14, 3,  5,  11, 4,   1,   3,  32,  1,  19,  4,   1,   13, 11,  5,  2,   1,
		8,  6,  8,   1,  6,   5,   13, 3,  23, 11, 5,   3,   16, 3,   9,  10,  1,   24,  3,  198, 52, 4,   2,
		2,  5,  14,  5,  4,   22,  5,  20, 4,  11, 6,   41,  1,  5,   2,  2,   11,  5,   2,  28,  35, 8,   22,
		3,  18, 3,   10, 7,   5,   3,  4,  1,  5,  3,   8,   9,  3,   6,  2,   16,  22,  4,  5,   5,  3,   3,
		18, 23, 2,   6,  23,  5,   27, 8,  1,  33, 2,   12,  43, 16,  5,  2,   3,   6,   1,  20,  4,  2,   9,
		7,  1,  11,  2,  10,  3,   14, 31, 9,  3,  25,  18,  20, 2,   5,  5,   26,  14,  1,  11,  17, 12,  40,
		19, 9,  6,   31, 83,  2,   7,  9,  19, 78, 12,  14,  21, 76,  12, 113, 79,  34,  4,  1,   1,  61,  18,
		85, 10, 2,   2,  13,  31,  11, 50, 6,  33, 159, 179, 6,  6,   7,  4,   4,   2,   4,  2,   5,  8,   7,
		20, 32, 22,  1,  3,   10,  6,  7,  28, 5,  10,  9,   2,  77,  19, 13,  2,   5,   1,  4,   4,  7,   4,
		13, 3,  9,   31, 17,  3,   26, 2,  6,  6,  5,   4,   1,  7,   11, 3,   4,   2,   1,  6,   2,  20,  4,
		1,  9,  2,   6,  3,   7,   1,  1,  1,  20, 2,   3,   1,  6,   2,  3,   6,   2,   4,  8,   1,  5,   13,
		8,  4,  11,  23, 1,   10,  6,  2,  1,  3,  21,  2,   2,  4,   24, 31,  4,   10,  10, 2,   5,  192, 15,
		4,  16, 7,   9,  51,  1,   2,  1,  1,  5,  1,   1,   2,  1,   3,  5,   3,   1,   3,  4,   1,  3,   1,
		3,  3,  9,   8,  1,   2,   2,  2,  4,  4,  18,  12,  92, 2,   10, 4,   3,   14,  5,  25,  16, 42,  4,
		14, 4,  2,   21, 5,   126, 30, 31, 2,  1,  5,   13,  3,  22,  5,  6,   6,   20,  12, 1,   14, 12,  87,
		3,  19, 1,   8,  2,   9,   9,  3,  3,  23, 2,   3,   7,  6,   3,  1,   2,   3,   9,  1,   3,  1,   6,
		3,  2,  1,   3,  11,  3,   1,  6,  10, 3,  2,   3,   1,  2,   1,  5,   1,   1,   11, 3,   6,  4,   1,
		7,  2,  1,   2,  5,   5,   34, 4,  14, 18, 4,   19,  7,  5,   8,  2,   6,   79,  1,  5,   2,  14,  8,
		2,  9,  2,   1,  36,  28,  16, 4,  1,  1,  1,   2,   12, 6,   42, 39,  16,  23,  7,  15,  15, 3,   2,
		12, 7,  21,  64, 6,   9,   28, 8,  12, 3,  3,   41,  59, 24,  51, 55,  57,  294, 9,  9,   2,  6,   2,
		15, 1,  2,   13, 38,  90,  9,  9,  9,  3,  11,  7,   1,  1,   1,  5,   6,   3,   2,  1,   2,  2,   3,
		8,  1,  4,   4,  1,   5,   7,  1,  4,  3,  20,  4,   9,  1,   1,  1,   5,   5,   17, 1,   5,  2,   6,
		2,  4,  1,   4,  5,   7,   3,  18, 11, 11, 32,  7,   5,  4,   7,  11,  127, 8,   4,  3,   3,  1,   10,
		1,  1,  6,   21, 14,  1,   16, 1,  7,  1,  3,   6,   9,  65,  51, 4,   3,   13,  3,  10,  1,  1,   12,
		9,  21, 110, 3,  19,  24,  1,  1,  10, 62, 4,   1,   29, 42,  78, 28,  20,  18,  82, 6,   3,  15,  6,
		84, 58, 253, 15, 155, 264, 15, 21, 9,  14, 7,   58,  40, 39,
	};
	static font_wchar base_ranges[] = // not zero-terminated
		{
			0x0020, 0x00FF, // Basic Latin + Latin Supplement
			0x3000, 0x30FF, // CJK Symbols and Punctuations, Hiragana, Katakana
			0x31F0, 0x31FF, // Katakana Phonetic Extensions
			0xFF00, 0xFFEF  // Half-width characters
		};
	static font_wchar full_ranges[countof(base_ranges) + countof(accumulative_offsets) * 2 + 1] = {0};
	if(!full_ranges[0])
	{
		memcpy(full_ranges, base_ranges, sizeof(base_ranges));
		unpack_accumulative_offsets_into_ranges(0x4E00, accumulative_offsets, countof(accumulative_offsets),
												full_ranges + countof(base_ranges));
	}
	return &full_ranges[0];
}

const font_wchar* get_glyph_ranges_cyrillic()
{
	static const font_wchar ranges[] = {
		0x0020, 0x00FF, // Basic Latin + Latin Supplement
		0x0400, 0x052F, // Cyrillic + Cyrillic Supplement
		0x2DE0, 0x2DFF, // Cyrillic Extended-A
		0xA640, 0xA69F, // Cyrillic Extended-B
		0,
	};
	return &ranges[0];
}

const font_wchar* get_glyph_ranges_thai()
{
	static const font_wchar ranges[] = {
		0x0020, 0x00FF, // Basic Latin
		0x2010, 0x205E, // Punctuations
		0x0E00, 0x0E7F, // Thai
		0,
	};
	return &ranges[0];
}

const font_wchar* get_glyph_ranges_vietnamese()
{
	static const font_wchar ranges[] = {
		0x0020, 0x00FF, // Basic Latin
		0x0102, 0x0103, //
		0x0110, 0x0111, //
		0x0128, 0x0129, //
		0x0168, 0x0169, //
		0x01A0, 0x01A1, //
		0x01AF, 0x01B0, //
		0x1EA0, 0x1EF9, //
		0,
	};
	return &ranges[0];
}

//-----------------------------------------------------------------------------
// [SECTION] font_glyph_ranges_builder
//-----------------------------------------------------------------------------

void font_glyph_ranges_builder::add_text(const char* text, const char* text_end)
{
	while(text_end ? (text < text_end) : *text)
	{
		unsigned int c = 0;
		int c_len = text_char_from_utf8(&c, text, text_end);
		text += c_len;
		if(c_len == 0)
			break;
		if(c < 0x10000)
			add_char(font_wchar(c));
	}
}

void font_glyph_ranges_builder::add_ranges(const font_wchar* ranges)
{
	for(; ranges[0]; ranges += 2)
		for(font_wchar c = ranges[0]; c <= ranges[1]; c++)
			add_char(c);
}

std::vector<font_wchar> font_glyph_ranges_builder::build_ranges()
{
	std::vector<font_wchar> out_ranges;
	int max_codepoint = 0x10000;
	for(int n = 0; n < max_codepoint; n++)
		if(get_bit(n))
		{
			out_ranges.push_back(font_wchar(n));
			while(n < max_codepoint - 1 && get_bit(n + 1))
				n++;
			out_ranges.push_back(font_wchar(n));
		}
	out_ranges.push_back(0);
	return out_ranges;
}

//-----------------------------------------------------------------------------
// [SECTION] font_info
//-----------------------------------------------------------------------------

font_info::~font_info()
{
	clear_output_data();
}

void font_info::clear_output_data()
{
	font_size = 0.0f;
	fallback_advance_x = 0.0f;
	glyphs.clear();
	index_advance_x.clear();
	index_lookup.clear();
	fallback_glyph = nullptr;
	container_atlas = nullptr;
	dirty_lookup_tables = true;
	ascent = descent = 0.0f;
	metrics_total_surface = 0;
}

void font_info::build_lookup_table()
{
	size_t max_codepoint = 0;
	for(const auto& glyph : glyphs)
		max_codepoint = std::max(max_codepoint, size_t(glyph.codepoint));

	assert(glyphs.size() < 0xFFFF); // -1 is reserved
	index_advance_x.clear();
	index_lookup.clear();
	dirty_lookup_tables = false;
	grow_index(max_codepoint + 1);
	for(size_t i = 0; i < glyphs.size(); i++)
	{
		auto codepoint = size_t(glyphs[i].codepoint);
		index_advance_x[codepoint] = glyphs[i].advance_x;
		index_lookup[codepoint] = font_wchar(i);
	}

	// Create a glyph to handle TAB
	// FIXME: Needs proper TAB handling but it needs to be contextualized (or we could arbitrary say that each
	// string starts at "column 0" ?)
	auto tab_glyph_found = find_glyph(font_wchar(' '));
	if(tab_glyph_found)
	{
		if(glyphs.back().codepoint != '\t') // So we can call this function multiple times
			glyphs.resize(glyphs.size() + 1);
		font_glyph& tab_glyph = glyphs.back();
		tab_glyph = *tab_glyph_found;
		tab_glyph.codepoint = '\t';
		tab_glyph.advance_x *= IM_TABSIZE;
		index_advance_x[size_t(tab_glyph.codepoint)] = tab_glyph.advance_x;
		index_lookup[size_t(tab_glyph.codepoint)] = font_wchar(glyphs.size() - 1);
	}

	fallback_glyph = find_glyph_no_fallback(fallback_char);
	fallback_advance_x = fallback_glyph ? fallback_glyph->advance_x : 0.0f;
	for(size_t i = 0; i < max_codepoint + 1; i++)
		if(index_advance_x[i] < 0.0f)
			index_advance_x[i] = fallback_advance_x;

    if(x_height <= 0.0f)
    {
        x_height = ascent - descent;
    }
}

void font_info::set_fallback_char(font_wchar c)
{
	fallback_char = c;
	build_lookup_table();
}

void font_info::grow_index(size_t new_size)
{
	assert(index_advance_x.size() == index_lookup.size());
	if(new_size <= index_lookup.size())
		return;
	index_advance_x.resize(new_size, -1.0f);
	index_lookup.resize(new_size, font_wchar(-1));
}

// x0/y0/x1/y1 are offset from the character upper-left layout position, in pixels. Therefore x0/y0 are often
// fairly close to zero. Not to be mistaken with texture coordinates, which are held by u0/v0/u1/v1 in
// normalized format (0.0..1.0 on each texture axis).
void font_info::add_glyph(font_wchar codepoint, float x0, float y0, float x1, float y1, float u0, float v0,
						  float u1, float v1, float advance_x)
{
	glyphs.resize(glyphs.size() + 1);
	font_glyph& glyph = glyphs.back();
	glyph.codepoint = codepoint;
	glyph.x0 = x0;
	glyph.y0 = y0;
	glyph.x1 = x1;
	glyph.y1 = y1;
	glyph.u0 = u0;
	glyph.v0 = v0;
	glyph.u1 = u1;
	glyph.v1 = v1;
	glyph.advance_x = advance_x + config_data->glyph_extra_spacing_x; // Bake spacing into AdvanceX

	if(config_data->pixel_snap_h)
		glyph.advance_x = static_cast<float>(static_cast<int>(glyph.advance_x + 0.5f));

	// Compute rough surface usage metrics (+1 to account for average padding, +0.99 to round)
	dirty_lookup_tables = true;
	metrics_total_surface += static_cast<int>((glyph.u1 - glyph.u0) * container_atlas->tex_width + 1.99f) *
							 static_cast<int>((glyph.v1 - glyph.v0) * container_atlas->tex_height + 1.99f);
}

void font_info::add_remap_char(font_wchar dst, font_wchar src, bool overwrite_dst)
{
	assert(!index_lookup.empty()); // Currently this can only be called AFTER the font has been built, aka
								   // after calling font_atlas::GetTexDataAs*() function.
	size_t index_size = index_lookup.size();

	if(dst < index_size && index_lookup[dst] == font_wchar(-1) && !overwrite_dst) // 'dst' already exists
		return;
	if(src >= index_size && dst >= index_size) // both 'dst' and 'src' don't exist -> no-op
		return;

	grow_index(dst + 1);
	index_lookup[dst] = (src < index_size) ? index_lookup[src] : font_wchar(-1);
	index_advance_x[dst] = (src < index_size) ? index_advance_x[src] : 1.0f;
}

const font_glyph* font_info::find_glyph(font_wchar c) const
{
	if(c >= index_lookup.size())
		return fallback_glyph;
	const font_wchar i = index_lookup[c];
	if(i == font_wchar(-1))
		return fallback_glyph;
	return &glyphs[i];
}

const font_glyph* font_info::find_glyph_no_fallback(font_wchar c) const
{
	if(c >= index_lookup.size())
		return nullptr;
	const font_wchar i = index_lookup[c];
	if(i == font_wchar(-1))
		return nullptr;
	return &glyphs[i];
}

//-----------------------------------------------------------------------------
// [SECTION] Default font data (ProggyClean.ttf)
//-----------------------------------------------------------------------------
// ProggyClean.ttf
// Copyright (c) 2004, 2005 Tristan Grimmer
// MIT license (see License.txt in http://www.upperbounds.net/download/ProggyClean.ttf.zip)
// Download and more information at http://upperbounds.net
//-----------------------------------------------------------------------------
// File: 'ProggyClean.ttf' (41208 bytes)
// Exported using misc/fonts/binary_to_compressed_c.cpp (with compression + base85 string encoding).
// The purpose of encoding as base85 instead of "0x00,0x01,..." style is only save on _source code_ size.
//-----------------------------------------------------------------------------
static const char proggy_clean_ttf_compressed_data_base85[11980 + 1] =
	"7])#######hV0qs'/###[),##/l:$#Q6>##5[n42>c-TH`->>#/"
	"e>11NNV=Bv(*:.F?uu#(gRU.o0XGH`$vhLG1hxt9?W`#,5LsCp#-i>.r$<$6pD>Lb';9Crc6tgXmKVeU2cD4Eo3R/"
	"2*>]b(MC;$jPfY.;h^`IWM9<Lh2TlS+f-s$o6Q<BWH`YiU.xfLq$N;$0iR/GX:U(jcW2p/"
	"W*q?-qmnUCI;jHSAiFWM.R*kU@C=GH?a9wp8f$e.-4^Qg1)Q-GL(lf(r/7GrRgwV%MS=C#"
	"`8ND>Qo#t'X#(v#Y9w0#1D$CIf;W'#pWUPXOuxXuU(H9M(1<q-UE31#^-V'8IRUo7Qf./"
	"L>=Ke$$'5F%)]0^#0X@U.a<r:QLtFsLcL6##lOj)#.Y5<-R&KgLwqJfLgN&;Q?gI^#DY2uL"
	"i@^rMl9t=cWq6##weg>$FBjVQTSDgEKnIS7EM9>ZY9w0#L;>>#Mx&4Mvt//"
	"L[MkA#W@lK.N'[0#7RL_&#w+F%HtG9M#XL`N&.,GM4Pg;-<nLENhvx>-VsM.M0rJfLH2eTM`*oJMHRC`N"
	"kfimM2J,W-jXS:)r0wK#@Fge$U>`w'N7G#$#fB#$E^$#:9:hk+eOe--6x)F7*E%?76%^GMHePW-Z5l'&GiF#$956:rS?dA#fiK:)Yr+`"
	"&#0j@'DbG&#^$PG.Ll+DNa<XCMKEV*N)LN/N"
	"*b=%Q6pia-Xg8I$<MR&,VdJe$<(7G;Ckl'&hF;;$<_=X(b.RS%%)###MPBuuE1V:v&cX&#2m#(&cV]`k9OhLMbn%s$G2,B$BfD3X*"
	"sp5#l,$R#]x_X1xKX%b5U*[r5iMfUo9U`N99hG)"
	"tm+/Us9pG)XPu`<0s-)WTt(gCRxIg(%6sfh=ktMKn3j)<6<b5Sk_/0(^]AaN#(p/L>&VZ>1i%h1S9u5o@YaaW$e+b<TWFn/"
	"Z:Oh(Cx2$lNEoN^e)#CFY@@I;BOQ*sRwZtZxRcU7uW6CX"
	"ow0i(?$Q[cjOd[P4d)]>ROPOpxTO7Stwi1::iB1q)C_=dV26J;2,]7op$]uQr@_V7$q^%lQwtuHY]=DX,n3L#0PHDO4f9>dC@O>"
	"HBuKPpP*E,N+b3L#lpR/MrTEH.IAQk.a>D[.e;mc."
	"x]Ip.PH^'/aqUO/$1WxLoW0[iLA<QT;5HKD+@qQ'NQ(3_PLhE48R.qAPSwQ0/WK?Z,[x?-J;jQTWA0X@KJ(_Y8N-:/M74:/"
	"-ZpKrUss?d#dZq]DAbkU*JqkL+nwX@@47`5>w=4h(9.`G"
	"CRUxHPeR`5Mjol(dUWxZa(>STrPkrJiWx`5U7F#.g*jrohGg`cg:lSTvEY/"
	"EV_7H4Q9[Z%cnv;JQYZ5q.l7Zeas:HOIZOB?G<Nald$qs]@]L<J7bR*>gv:[7MI2k).'2($5FNP&EQ(,)"
	"U]W]+fh18.vsai00);D3@4ku5P?DP8aJt+;qUM]=+b'8@;mViBKx0DE[-auGl8:PJ&Dj+M6OC]O^((##]`0i)drT;-7X`=-H3["
	"igUnPG-NZlo.#k@h#=Ork$m>a>$-?Tm$UV(?#P6YY#"
	"'/###xe7q.73rI3*pP/$1>s9)W,JrM7SN]'/"
	"4C#v$U`0#V.[0>xQsH$fEmPMgY2u7Kh(G%siIfLSoS+MK2eTM$=5,M8p`A.;_R%#u[K#$x4AG8.kK/HSB==-'Ie/QTtG?-.*^N-4B/ZM"
	"_3YlQC7(p7q)&](`6_c)$/*JL(L-^(]$wIM`dPtOdGA,U3:w2M-0<q-]L_?^)1vw'.,MRsqVr.L;aN&#/"
	"EgJ)PBc[-f>+WomX2u7lqM2iEumMTcsF?-aT=Z-97UEnXglEn1K-bnEO`gu"
	"Ft(c%=;Am_Qs@jLooI&NX;]0#j4#F14;gl8-GQpgwhrq8'=l_f-b49'UOqkLu7-##oDY2L(te+Mch&gLYtJ,MEtJfLh'x'M=$CS-ZZ%"
	"P]8bZ>#S?YY#%Q&q'3^Fw&?D)UDNrocM3A76/"
	"/oL?#h7gl85[qW/"
	"NDOk%16ij;+:1a'iNIdb-ou8.P*w,v5#EI$TWS>Pot-R*H'-SEpA:g)f+O$%%`kA#G=8RMmG1&O`>to8bC]T&$,n.LoO>29sp3dt-"
	"52U%VM#q7'DHpg+#Z9%H[K<L"
	"%a2E-grWVM3@2=-k22tL]4$##6We'8UJCKE[d_=%wI;'6X-GsLX4j^SgJ$##R*w,vP3wK#iiW&#*h^D&R?jp7+/"
	"u&#(AP##XU8c$fSYW-J95_-Dp[g9wcO&#M-h1OcJlc-*vpw0xUX&#"
	"OQFKNX@QI'IoPp7nb,QU//"
	"MQ&ZDkKP)X<WSVL(68uVl&#c'[0#(s1X&xm$Y%B7*K:eDA323j998GXbA#pwMs-jgD$9QISB-A_(aN4xoFM^@C58D0+Q+q3n0#"
	"3U1InDjF682-SjMXJK)("
	"h$hxua_K]ul92%'BOU&#BRRh-slg8KDlr:%L71Ka:.A;%YULjDPmL<LYs8i#XwJOYaKPKc1h:'9Ke,g)b),78=I39B;xiY$bgGw-&."
	"Zi9InXDuYa%G*f2Bq7mn9^#p1vv%#(Wi-;/Z5h"
	"o;#2:;%d&#x9v68C5g?ntX0X)pT`;%pB3q7mgGN)3%(P8nTd5L7GeA-GL@+%J3u2:(Yf>et`e;)f#Km8&+DC$I46>#Kr]]u-[="
	"99tts1.qb#q72g1WJO81q+eN'03'eM>&1XxY-caEnO"
	"j%2n8)),?ILR5^.Ibn<-X-Mq7[a82Lq:F&#ce+S9wsCK*x`569E8ew'He]h:sI[2LM$[guka3ZRd6:t%IG:;$%YiJ:Nq=?eAw;/"
	":nnDq0(CYcMpG)qLN4$##&J<j$UpK<Q4a1]MupW^-"
	"sj_$%[HK%'F####QRZJ::Y3EGl4'@%FkiAOg#p[##O`gukTfBHagL<LHw%q&OV0##F=6/"
	":chIm0@eCP8X]:kFI%hl8hgO@RcBhS-@Qb$%+m=hPDLg*%K8ln(wcf3/'DW-$.lR?n[nCH-"
	"eXOONTJlh:.RYF%3'p6sq:UIMA945&^HFS87@$EP2iG<-lCO$%c`uKGD3rC$x0BL8aFn--`ke%#HMP'vh1/"
	"R&O_J9'um,.<tx[@%wsJk&bUT2`0uMv7gg#qp/ij.L56'hl;.s5CUrxjO"
	"M7-##.l+Au'A&O:-T72L]P`&=;ctp'XScX*rU.>-XTt,%OVU4)S1+R-#dg0/"
	"Nn?Ku1^0f$B*P:Rowwm-`0PKjYDDM'3]d39VZHEl4,.j']Pk-M.h^&:0FACm$maq-&sgw0t7/6(^xtk%"
	"LuH88Fj-ekm>GA#_>568x6(OFRl-IZp`&b,_P'$M<Jnq79VsJW/mWS*PUiq76;]/NM_>hLbxfc$mj`,O;&%W2m`Zh:/"
	")Uetw:aJ%]K9h:TcF]u_-Sj9,VK3M.*'&0D[Ca]J9gp8,kAW]"
	"%(?A%R$f<->Zts'^kn=-^@c4%-pY6qI%J%1IGxfLU9CP8cbPlXv);C=b),<2mOvP8up,UVf3839acAWAW-W?#ao/"
	"^#%KYo8fRULNd2.>%m]UK:n%r$'sw]J;5pAoO_#2mO3n,'=H5(et"
	"Hg*`+RLgv>=4U8guD$I%D:W>-r5V*%j*W:Kvej.Lp$<M-SGZ':+Q_k+uvOSLiEo(<aD/"
	"K<CCc`'Lx>'?;++O'>()jLR-^u68PHm8ZFWe+ej8h:9r6L*0//c&iH&R8pRbA#Kjm%upV1g:"
	"a_#Ur7FuA#(tRh#.Y5K+@?3<-8m0$PEn;J:rh6?I6uG<-`wMU'ircp0LaE_OtlMb&1#6T.#FDKu#1Lw%u%+GM+X'e?YLfjM["
	"VO0MbuFp7;>Q&#WIo)0@F%q7c#4XAXN-U&VB<HFF*qL("
	"$/V,;(kXZejWO`<[5?\?ewY(*9=%wDc;,u<'9t3W-(H1th3+G]ucQ]kLs7df($/"
	"*JL]@*t7Bu_G3_7mp7<iaQjO@.kLg;x3B0lqp7Hf,^Ze7-##@/c58Mo(3;knp0%)A7?-W+eI'o8)b<"
	"nKnw'Ho8C=Y>pqB>0ie&jhZ[?iLR@@_AvA-iQC(=ksRZRVp7`.=+NpBC%rh&3]R:8XDmE5^V8O(x<<aG/"
	"1N$#FX$0V5Y6x'aErI3I$7x%E`v<-BY,)%-?Psf*l?%C3.mM(=/M0:JxG'?"
	"7WhH%o'a<-80g0NBxoO(GH<dM]n.+%q@jH?f.UsJ2Ggs&4<-e47&Kl+f//"
	"9@`b+?.TeN_&B8Ss?v;^Trk;f#YvJkl&w$]>-+k?'(<S:68tq*WoDfZu';mM?8X[ma8W%*`-=;D.(nc7/;"
	")g:T1=^J$&BRV(-lTmNB6xqB[@0*o.erM*<SWF]u2=st-*(6v>^](H.aREZSi,#1:[IXaZFOm<-ui#qUq2$##Ri;u75OK#(RtaW-K-F`"
	"S+cF]uN`-KMQ%rP/Xri.LRcB##=YL3BgM/3M"
	"D?@f&1'BW-)Ju<L25gl8uhVm1hL$##*8###'A3/LkKW+(^rWX?5W_8g)a(m&K8P>#bmmWCMkk&#TR`C,5d>g)F;t,4:@_l8G/"
	"5h4vUd%&%950:VXD'QdWoY-F$BtUwmfe$YqL'8(PWX("
	"P?^@Po3$##`MSs?DWBZ/S>+4%>fX,VWv/w'KD`LP5IbH;rTV>n3cEK8U#bX]l-/"
	"V+^lj3;vlMb&[5YQ8#pekX9JP3XUC72L,,?+Ni&co7ApnO*5NK,((W-i:$,kp'UDAO(G0Sq7MVjJs"
	"bIu)'Z,*[>br5fX^:FPAWr-m2KgL<LUN098kTF&#lvo58=/vjDo;.;)Ka*hLR#/"
	"k=rKbxuV`>Q_nN6'8uTG&#1T5g)uLv:873UpTLgH+#FgpH'_o1780Ph8KmxQJ8#H72L4@768@Tm&Q"
	"h4CB/5OvmA&,Q&QbUoi$a_%3M01H)4x7I^&KQVgtFnV+;[Pc>[m4k//"
	",]1?#`VY[Jr*3&&slRfLiVZJ:]?=K3Sw=[$=uRB?3xk48@aeg<Z'<$#4H)6,>e0jT6'N#(q%.O=?2S]u*(m<-"
	"V8J'(1)G][68hW$5'q[GC&5j`TE?m'esFGNRM)j,ffZ?-qx8;->g4t*:CIP/[Qap7/"
	"9'#(1sao7w-.qNUdkJ)tCF&#B^;xGvn2r9FEPFFFcL@.iFNkTve$m%#QvQS8U@)2Z+3K:AKM5i"
	"sZ88+dKQ)W6>J%CL<KE>`.d*(B`-n8D9oK<Up]c$X$(,)M8Zt7/"
	"[rdkqTgl-0cuGMv'?>-XV1q['-5k'cAZ69e;D_?$ZPP&s^+7])$*$#@QYi9,5P&#9r+$%CE=68>K8r0=dSC%%(@p7"
	".m7jilQ02'0-VWAg<a/''3u.=4L$Y)6k/K:_[3=&jvL<L0C/"
	"2'v:^;-DIBW,B4E68:kZ;%?8(Q8BH=kO65BW?xSG&#@uU,DS*,?.+(o(#1vCS8#CHF>TlGW'b)Tq7VT9q^*^$$.:&N@@"
	"$&)WHtPm*5_rO0&e%K&#-30j(E4#'Zb.o/"
	"(Tpm$>K'f@[PvFl,hfINTNU6u'0pao7%XUp9]5.>%h`8_=VYbxuel.NTSsJfLacFu3B'lQSu/m6-Oqem8T+oE--$0a/"
	"k]uj9EwsG>%veR*"
	"hv^BFpQj:K'#SJ,sB-'#](j.Lg92rTw-*n%@/;39rrJF,l#qV%OrtBeC6/"
	",;qB3ebNW[?,Hqj2L.1NP&GjUR=1D8QaS3Up&@*9wP?+lo7b?@%'k4`p0Z$22%K3+iCZj?XJN4Nm&+YF]u"
	"@-W$U%VEQ/,,>>#)D<h#`)h0:<Q6909ua+&VU%n2:cG3FJ-%@Bj-DgLr`Hw&HAKjKjseK</"
	"xKT*)B,N9X3]krc12t'pgTV(Lv-tL[xg_%=M_q7a^x?7Ubd>#%8cY#YZ?=,`Wdxu/ae&#"
	"w6)R89tI#6@s'(6Bf7a&?S=^ZI_kS&ai`&=tE72L_D,;^R)7[$s<Eh#c&)q.MXI%#v9ROa5FZO%sF7q7Nwb&#ptUJ:aqJe$Sl68%.D##"
	"#EC><?-aF&#RNQv>o8lKN%5/$(vdfq7+ebA#"
	"u1p]ovUKW&Y%q]'>$1@-[xfn$7ZTp7mM,G,Ko7a&Gu%G[RMxJs[0MM%wci.LFDK)(<c`Q8N)jEIF*+?P2a8g%)$q]o2aH8C&<SibC/"
	"q,(e:v;-b#6[$NtDZ84Je2KNvB#$P5?tQ3nt(0"
	"d=j.LQf./"
	"Ll33+(;q3L-w=8dX$#WF&uIJ@-bfI>%:_i2B5CsR8&9Z&#=mPEnm0f`<&c)QL5uJ#%u%lJj+D-r;BoF&#4DoS97h5g)E#o:&S4weDF,"
	"9^Hoe`h*L+_a*NrLW-1pG_&2UdB8"
	"6e%B/:=>)N4xeW.*wft-;$'58-ESqr<b?UI(_%@[P46>#U`'6AQ]m&6/"
	"`Z>#S?YY#Vc;r7U2&326d=w&H####?TZ`*4?&.MK?LP8Vxg>$[QXc%QJv92.(Db*B)gb*BM9dM*hJMAo*c&#"
	"b0v=Pjer]$gG&JXDf->'StvU7505l9$AFvgYRI^&<^b68?j#q9QX4SM'RO#&sL1IM.rJfLUAj221]d##DW=m83u5;'bYx,*Sl0hL(W;;"
	"$doB&O/TQ:(Z^xBdLjL<Lni;''X.`$#8+1GD"
	":k$YUWsbn8ogh6rxZ2Z9]%nd+>V#*8U_72Lh+2Q8Cj0i:6hp&$C/:p(HK>T8Y[gHQ4`4)'$Ab(Nof%V'8hL&#<NEdtg(n'=S1A(Q1/"
	"I&4([%dM`,Iu'1:_hL>SfD07&6D<fp8dHM7/g+"
	"tlPN9J*rKaPct&?'uBCem^jn%9_K)<,C5K3s=5g&GmJb*[SYq7K;TRLGCsM-$$;S%:Y@r7AK0pprpL<Lrh,q7e/"
	"%KWK:50I^+m'vi`3?%Zp+<-d+$L-Sv:@.o19n$s0&39;kn;S%BSq*"
	"$3WoJSCLweV[aZ'MQIjO<7;X-X;&+dMLvu#^UsGEC9WEc[X(wI7#2.(F0jV*eZf<-Qv3J-c+J5AlrB#$p(H68LvEA'q3n0#m,[`*8Ft)"
	"FcYgEud]CWfm68,(aLA$@EFTgLXoBq/UPlp7"
	":d[/;r_ix=:TF`S5H-b<LI&HY(K=h#)]Lk$K14lVfm:x$H<3^Ql<M`$OhapBnkup'D#L$Pb_`N*g]2e;X/"
	"Dtg,bsj&K#2[-:iYr'_wgH)NUIR8a1n#S?Yej'h8^58UbZd+^FKD*T@;6A"
	"7aQC[K8d-(v6GI$x:T<&'Gp5Uf>@M.*J:;$-rv29'M]8qMv-tLp,'886iaC=Hb*YJoKJ,(j%K=H`K.v9HggqBIiZu'QvBT.#=)"
	"0ukruV&.)3=(^1`o*Pj4<-<aN((^7('#Z0wK#5GX@7"
	"u][`*S^43933A4rl][`*O4CgLEl]v$1Q3AeF37dbXk,.)vj#x'd`;qgbQR%FW,2(?LO=s%Sc68%NP'##Aotl8x=BE#j1UD([3$M(]"
	"UI2LX3RpKN@;/#f'f/&_mt&F)XdF<9t4)Qa.*kT"
	"LwQ'(TTB9.xH'>#MJ+gLq9-##@HuZPN0]u:h7.T..G:;$/"
	"Usj(T7`Q8tT72LnYl<-qx8;-HV7Q-&Xdx%1a,hC=0u+HlsV>nuIQL-5<N?)NBS)QN*_I,?&)2'IM%L3I)X((e/dl2&8'<M"
	":^#M*Q+[T.Xri.LYS3v%fF`68h;b-X[/En'CR.q7E)p'/"
	"kle2HM,u;^%OKC-N+Ll%F9CF<Nf'^#t2L,;27W:0O@6##U6W7:$rJfLWHj$#)woqBefIZ.PK<b*t7ed;p*_m;4ExK#h@&]>"
	"_>@kXQtMacfD.m-VAb8;IReM3$wf0''hra*so568'Ip&vRs849'MRYSp%:t:h5qSgwpEr$B>Q,;s(C#$)`svQuF$##-D,##,g68@2[T;"
	".XSdN9Qe)rpt._K-#5wF)sP'##p#C0c%-Gb%"
	"hd+<-j'Ai*x&&HMkT]C'OSl##5RG[JXaHN;d'uA#x._U;.`PU@(Z3dt4r152@:v,'R.Sj'w#0<-;kPI)FfJ&#AYJ&#//"
	")>-k=m=*XnK$>=)72L]0I%>.G690a:$##<,);?;72#?x9+d;"
	"^V'9;jY@;)br#q^YQpx:X#Te$Z^'=-=bGhLf:D6&bNwZ9-ZD#n^9HhLMr5G;']d&6'wYmTFmL<LD)F^%[tC'8;+9E#C$g%#5Y>q9wI>"
	"P(9mI[>kC-ekLC/R&CH+s'B;K-M6$EB%is00:"
	"+A4[7xks.LrNk0&E)wILYF@2L'0Nb$+pv<(2.768/"
	"FrY&h$^3i&@+G%JT'<-,v`3;_)I9M^AE]CN?Cl2AZg+%4iTpT3<n-&%H%b<FDj2M<hH=&Eh<2Len$b*aTX=-8QxN)k11IM1c^j%"
	"9s<L<NFSo)B?+<-(GxsF,^-Eh@$4dXhN$+#rxK8'je'D7k`e;)2pYwPA'_p9&@^18ml1^[@g4t*[JOa*[=Qp7(qJ_oOL^('7fB&Hq-:"
	"sf,sNj8xq^>$U4O]GKx'm9)b@p7YsvK3w^YR-"
	"CdQ*:Ir<($u&)#(&?L9Rg3H)4fiEp^iI9O8KnTj,]H?D*r7'M;PwZ9K0E^k&-cpI;.p/6_vwoFMV<->#%Xi.LxVnrU(4&8/"
	"P+:hLSKj$#U%]49t'I:rgMi'FL@a:0Y-uA[39',(vbma*"
	"hU%<-SRF`Tt:542R_VV$p@[p8DV[A,?1839FWdF<TddF<9Ah-6&9tWoDlh]&1SpGMq>Ti1O*H&#(AL8[_P%.M>v^-))qOT*F5Cq0`Ye%"
	"+$B6i:7@0IX<N+T+0MlMBPQ*Vj>SsD<U4JHY"
	"8kD2)2fU/M#$e.)T4,_=8hLim[&);?UkK'-x?'(:siIfL<$pFM`i<?%W(mGDHM%>iWP,##P`%/L<eXi:@Z9C.7o=@(pXdAO/"
	"NLQ8lPl+HPOQa8wD8=^GlPa8TKI1CjhsCTSLJM'/Wl>-"
	"S(qw%sf/@%#B6;/"
	"U7K]uZbi^Oc^2n<bhPmUkMw>%t<)'mEVE''n`WnJra$^TKvX5B>;_aSEK',(hwa0:i4G?.Bci.(X[?b*($,=-n<.Q%`(X=?+@Am*Js0&"
	"=3bh8K]mL<LoNs'6,'85`"
	"0?t/'_U59@]ddF<#LdF<eWdF<OuN/45rY<-L@&#+fm>69=Lb,OcZV/);TTm8VI;?%OtJ<(b4mq7M6:u?KRdF<gR@2L=FNU-<b[(9c/"
	"ML3m;Z[$oF3g)GAWqpARc=<ROu7cL5l;-[A]%/"
	"+fsd;l#SafT/"
	"f*W]0=O'$(Tb<[)*@e775R-:Yob%g*>l*:xP?Yb.5)%w_I?7uk5JC+FS(m#i'k.'a0i)9<7b'fs'59hq$*5Uhv##pi^8+hIEBF`nvo`;"
	"'l0.^S1<-wUK2/Coh58KKhLj"
	"M=SO*rfO`+qC`W-On.=AJ56>>i2@2LH6A:&5q`?9I3@@'04&p2/"
	"LVa*T-4<-i3;M9UvZd+N7>b*eIwg:CC)c<>nO&#<IGe;__.thjZl<%w(Wk2xmp4Q@I#I9,DF]u7-P=.-_:YJ]aS@V"
	"?6*C()dOp7:WL,b&3Rg/"
	".cmM9&r^>$(>.Z-I&J(Q0Hd5Q%7Co-b`-c<N(6r@ip+AurK<m86QIth*#v;-OBqi+L7wDE-Ir8K['m+DDSLwK&/"
	".?-V%U_%3:qKNu$_b*B-kp7NaD'QdWQPK"
	"Yq[@>P)hI;*_F]u`Rb[.j8_Q/<&>uu+VsH$sM9TA%?)(vmJ80),P7E>)tjD%2L=-t#fK[%`v=Q8<FfNkgg^oIbah*#8/Qt$F&:K*-(N/"
	"'+1vMB,u()-a.VUU*#[e%gAAO(S>WlA2);Sa"
	">gXm8YB`1d@K#n]76-a$U,mF<fX]idqd)<3,]J7JmW4`6]uks=4-72L(jEk+:bJ0M^q-8Dm_Z?0olP1C9Sa&H[d&c$ooQUj]Exd*3ZM@"
	"-WGW2%s',B-_M%>%Ul:#/'xoFM9QX-$.QN'>"
	"[%$Z$uF6pA6Ki2O5:8w*vP1<-1`[G,)-m#>0`P&#eb#.3i)rtB61(o'$?X3B</"
	"R90;eZ]%Ncq;-Tl]#F>2Qft^ae_5tKL9MUe9b*sLEQ95C&`=G?@Mj=wh*'3E>=-<)Gt*Iw)'QG:`@I"
	"wOf7&]1i'S01B+Ev/Nac#9S;=;YQpg_6U`*kVY39xK,[/"
	"6Aj7:'1Bm-_1EYfa1+o&o4hp7KN_Q(OlIo@S%;jVdn0'1<Vc52=u`3^o-n1'g4v58Hj&6_t7$##?M)c<$bgQ_'SY((-xkA#"
	"Y(,p'H9rIVY-b,'%bCPF7.J<Up^,(dU1VY*5#WkTU>h19w,WQhLI)3S#f$2(eb,jr*b;3Vw]*7NH%$c4Vs,eD9>XW8?N]o+(*pgC%/"
	"72LV-u<Hp,3@e^9UB1J+ak9-TN/mhKPg+AJYd$"
	"MlvAF_jCK*.O-^(63adMT->W%iewS8W6m2rtCpo'RS1R84=@paTKt)>=%&1[)*vp'u+x,VrwN;&]kuO9JDbg=pO$J*.jVe;u'm0dr9l,"
	"<*wMK*Oe=g8lV_KEBFkO'oU]^=[-792#ok,)"
	"i]lR8qQ2oA8wcRCZ^7w/Njh;?.stX?Q1>S1q4Bn$)K1<-rGdO'$Wr.Lc.CG)$/*JL4tNR/"
	",SVO3,aUw'DJN:)Ss;wGn9A32ijw%FL+Z0Fn.U9;reSq)bmI32U==5ALuG&#Vf1398/pVo"
	"1*c-(aY168o<`JsSbk-,1N;$>0:OUas(3:8Z972LSfF8eb=c-;>SPw7.6hn3m`9^Xkn(r.qS[0;T%&Qc=+STRxX'q1BNk3&*eu2;&8q$"
	"&x>Q#Q7^Tf+6<(d%ZVmj2bDi%.3L2n+4W'$P"
	"iDDG)g,r%+?,$@?uou5tSe2aN_AQU*<h`e-GI7)?OK2A.d7_c)?wQ5AS@DL3r#7fSkgl6-++D:'A,uq7SvlB$pcpH'q3n0#_%dY#"
	"xCpr-l<F0NR@-##FEV6NTF6##$l84N1w?AO>'IAO"
	"URQ##V^Fv-XFbGM7Fl(N<3DhLGF%q.1rC$#:T__&Pi68%0xi_&[qFJ(77j_&JWoF.V735&T,[R*:xFR*K5>>#`bW-?4Ne_&6Ne_&6Ne_"
	"&n`kr-#GJcM6X;uM6X;uM(.a..^2TkL%oR(#"
	";u.T%fAr%4tJ8&><1=GHZ_+m9/#H1F^R#SC#*N=BA9(D?v[UiFY>>^8p,KKF.W]L29uLkLlu/"
	"+4T<XoIB&hx=T1PcDaB&;HH+-AFr?(m9HZV)FKS8JCw;SD=6[^/DZUL`EUDf]GGlG&>"
	"w$)F./^n3+rlo+DB;5sIYGNk+i1t-69Jg--0pao7Sm#K)pdHW&;LuDNH@H>#/"
	"X-TI(;P>#,Gc>#0Su>#4`1?#8lC?#<xU?#@.i?#D:%@#HF7@#LRI@#P_[@#Tkn@#Xw*A#]-=A#a9OA#"
	"d<F&#*;G##.GY##2Sl##6`($#:l:$#>xL$#B.`$#F:r$#JF.%#NR@%#R_R%#Vke%#Zww%#_-4&#3^Rh%Sflr-k'MS.o?.5/sWel/"
	"wpEM0%3'/1)K^f1-d>G21&v(35>V`39V7A4=onx4"
	"A1OY5EI0;6Ibgr6M$HS7Q<)58C5w,;WoA*#[%T*#`1g*#d=#+#hI5+#lUG+#pbY+#tnl+#x$),#&1;,#*=M,#.I`,#2Ur,#6b.-#;w["
	"H#iQtA#m^0B#qjBB#uvTB##-hB#'9$C#+E6C#"
	"/QHC#3^ZC#7jmC#;v)D#?,<D#C8ND#GDaD#KPsD#O]/"
	"E#g1A5#KA*1#gC17#MGd;#8(02#L-d3#rWM4#Hga1#,<w0#T.j<#O#'2#CYN1#qa^:#_4m3#o@/=#eG8=#t8J5#`+78#4uI-#"
	"m3B2#SB[8#Q0@8#i[*9#iOn8#1Nm;#^sN9#qh<9#:=x-#P;K2#$%X9#bC+.#Rg;<#mN=.#MTF.#RZO.#2?)4#Y#(/#[)1/#b;L/#dAU/"
	"#0Sv;#lY$0#n`-0#sf60#(F24#wrH0#%/e0#"
	"TmD<#%JSMFove:CTBEXI:<eh2g)B,3h2^G3i;#d3jD>)4kMYD4lVu`4m`:&5niUA5@(A5BA1]PBB:xlBCC=2CDLXMCEUtiCf&0g2'tN?"
	"PGT4CPGT4CPGT4CPGT4CPGT4CPGT4CPGT4CP"
	"GT4CPGT4CPGT4CPGT4CPGT4CPGT4CP-qekC`.9kEg^+F$kwViFJTB&5KTB&5KTB&5KTB&5KTB&5KTB&5KTB&5KTB&5KTB&5KTB&5KTB&"
	"5KTB&5KTB&5KTB&5KTB&5o,^<-28ZI'O?;xp"
	"O?;xpO?;xpO?;xpO?;xpO?;xpO?;xpO?;xpO?;xpO?;xpO?;xpO?;xpO?;xpO?;xp;7q-#lLYI:xvD=#";

static const char* get_default_compressed_font_data_ttf_base85()
{
	return proggy_clean_ttf_compressed_data_base85;
}

//-----------------------------------------------------------------------------
// [SECTION] Decompression code
//-----------------------------------------------------------------------------
// Compressed with stb_compress() then converted to a C array and encoded as base85.
// Use the program in misc/fonts/binary_to_compressed_c.cpp to create the array from a TTF file.
// The purpose of encoding as base85 instead of "0x00,0x01,..." style is only save on _source code_ size.
// Decompression from stb.h (public domain) by Sean Barrett https://github.com/nothings/stb/blob/master/stb.h
//-----------------------------------------------------------------------------

static unsigned int stb_decompress_length(const uint8_t* input)
{
	return static_cast<unsigned int>((input[8] << 24) + (input[9] << 16) + (input[10] << 8) + input[11]);
}

static uint8_t *stb__barrier_out_e, *stb__barrier_out_b;
static const uint8_t* stb__barrier_in_b;
static uint8_t* stb__dout;
static void stb__match(const uint8_t* data, unsigned int length)
{
	// INVERSE of memmove... write each byte before copying the next...
	assert(stb__dout + length <= stb__barrier_out_e);
	if(stb__dout + length > stb__barrier_out_e)
	{
		stb__dout += length;
		return;
	}
	if(data < stb__barrier_out_b)
	{
		stb__dout = stb__barrier_out_e + 1;
		return;
	}
	while(length--)
		*stb__dout++ = *data++;
}

static void stb__lit(const uint8_t* data, unsigned int length)
{
	assert(stb__dout + length <= stb__barrier_out_e);
	if(stb__dout + length > stb__barrier_out_e)
	{
		stb__dout += length;
		return;
	}
	if(data < stb__barrier_in_b)
	{
		stb__dout = stb__barrier_out_e + 1;
		return;
	}
	memcpy(stb__dout, data, length);
	stb__dout += length;
}

#define stb__in2(x) ((i[x] << 8) + i[(x) + 1])
#define stb__in3(x) ((i[x] << 16) + stb__in2((x) + 1))
#define stb__in4(x) ((i[x] << 24) + stb__in3((x) + 1))

static const uint8_t* stb_decompress_token(const uint8_t* i)
{
	if(*i >= 0x20)
	{ // use fewer if's for cases that expand small
		if(*i >= 0x80)
			stb__match(stb__dout - i[1] - 1, i[0] - 0x80 + 1), i += 2;
		else if(*i >= 0x40)
			stb__match(stb__dout - (stb__in2(0) - 0x4000 + 1), i[2] + 1), i += 3;
		else /* *i >= 0x20 */
			stb__lit(i + 1, i[0] - 0x20 + 1), i += 1 + (i[0] - 0x20 + 1);
	}
	else
	{ // more ifs for cases that expand large, since overhead is amortized
		if(*i >= 0x18)
			stb__match(stb__dout - (stb__in3(0) - 0x180000 + 1), i[3] + 1), i += 4;
		else if(*i >= 0x10)
			stb__match(stb__dout - (stb__in3(0) - 0x100000 + 1), stb__in2(3) + 1), i += 5;
		else if(*i >= 0x08)
			stb__lit(i + 2, stb__in2(0) - 0x0800 + 1), i += 2 + (stb__in2(0) - 0x0800 + 1);
		else if(*i == 0x07)
			stb__lit(i + 3, stb__in2(1) + 1), i += 3 + (stb__in2(1) + 1);
		else if(*i == 0x06)
			stb__match(stb__dout - (stb__in3(1) + 1), i[4] + 1), i += 5;
		else if(*i == 0x04)
			stb__match(stb__dout - (stb__in3(1) + 1), stb__in2(4) + 1), i += 6;
	}
	return i;
}

static unsigned int stb_adler32(unsigned int adler32, uint8_t* buffer, unsigned int buflen)
{
	const unsigned long adler_mod = 65521;
	unsigned long s1 = adler32 & 0xffff, s2 = adler32 >> 16;
	unsigned long blocklen, i;

	blocklen = buflen % 5552;
	while(buflen)
	{
		for(i = 0; i + 7 < blocklen; i += 8)
		{
			s1 += buffer[0];
			s2 += s1;
			s1 += buffer[1];
			s2 += s1;
			s1 += buffer[2];
			s2 += s1;
			s1 += buffer[3];
			s2 += s1;
			s1 += buffer[4];
			s2 += s1;
			s1 += buffer[5];
			s2 += s1;
			s1 += buffer[6];
			s2 += s1;
			s1 += buffer[7];
			s2 += s1;

			buffer += 8;
		}

		for(; i < blocklen; ++i)
		{
			s1 += *buffer++;
			s2 += s1;
		}
		s1 %= adler_mod, s2 %= adler_mod;
		buflen -= blocklen;
		blocklen = 5552;
	}
	return (unsigned int)(s2 << 16) + (unsigned int)s1;
}

static unsigned int stb_decompress(uint8_t* output, const uint8_t* i, unsigned int /*length*/)
{
	unsigned int olen;
	if(stb__in4(0) != 0x57bC0000)
		return 0;
	if(stb__in4(4) != 0)
		return 0; // error! stream is > 4GB
	olen = stb_decompress_length(i);
	stb__barrier_in_b = i;
	stb__barrier_out_e = output + olen;
	stb__barrier_out_b = output;
	i += 16;

	stb__dout = output;
	for(;;)
	{
		const uint8_t* old_i = i;
		i = stb_decompress_token(i);
		if(i == old_i)
		{
			if(*i == 0x05 && i[1] == 0xfa)
			{
				assert(stb__dout == output + olen);
				if(stb__dout != output + olen)
					return 0;
				if(stb_adler32(1, output, olen) != (unsigned int)stb__in4(2))
					return 0;
				return olen;
			}
			else
			{
				assert(0); /* NOTREACHED */
				return 0;
			}
		}
		assert(stb__dout <= output + olen);
		if(stb__dout > output + olen)
			return 0;
	}
}
} // namespace library_template
