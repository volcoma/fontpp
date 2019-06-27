#pragma once
#include "utils.h"
namespace fnt
{

//-----------------------------------------------------------------------------
// Font API
//-----------------------------------------------------------------------------
// Runtime data for a single font within a parent ImFontAtlas
struct font_info;
// Runtime data for multiple fonts, bake multiple fonts into a single texture, TTF/OTF font loader
struct font_atlas;
// Configuration data when adding a font or merging fonts
struct font_config;
// A single font glyph (code point + coordinates within in ImFontAtlas + offset)
struct font_glyph;
// Helper to build glyph ranges from text/string data
struct font_glyph_ranges_builder;
// A single U16 character for keyboard input/display.
// We encode them as multi bytes UTF-8 when used in strings.
using font_wchar = uint16_t;

struct pair_hash
{
	size_t operator()(const std::pair<font_wchar, font_wchar>& pair) const
	{
        static_assert(sizeof(std::size_t)>= (sizeof(font_wchar) * 2), "Ensure that std::size_t, the type of the hash, is large enough");
		return uint32_t(pair.first) << (sizeof(font_wchar) * 8) | uint32_t(pair.second);
	}
};

using kerning_table = std::unordered_map<std::pair<font_wchar, font_wchar>, float, pair_hash>;

enum class font_rasterizer
{
	stb,
	freetype
};

enum font_atlas_flags : int
{
	none = 0,
	no_power_of_two_height = 1 << 0, // Don't round the height to next power of two
};

struct font_config
{
	/// TTF/OTF data
	void* font_data{};
	/// TTF/OTF data size
	size_t font_data_size{};
	/// TTF/OTF data ownership taken by the container ImFontAtlas (will delete memory itself).
	bool font_data_owned_by_atlas{true};
	/// Index of font within TTF/OTF file
	int font_no{};
	/// Size in pixels for rasterizer (more or less maps to the resulting font height).
	float size_pixels{};
	/// Rasterize at higher quality for sub-pixel positioning. Read
	/// https://github.com/nothings/stb/blob/master/tests/oversample/README.md for details.
	int oversample_h{1};
	/// Rasterize at higher quality for sub-pixel positioning. We don't use sub-pixel positions on the Y axis.
	int oversample_v{1};
	/// Align every glyph to pixel boundary. Useful e.g. if you are merging a non-pixel aligned font with the
	/// default font. If enabled, you can set OversampleH/V to 1.
	bool pixel_snap_h{};
	/// Extra spacing (in pixels) between glyphs. Only X axis is supported for now.
	float glyph_extra_spacing_x{};
	float glyph_extra_spacing_y{};
	/// Offset all glyphs from this font input.
	float glyph_offset_x{};
	float glyph_offset_y{};
	/// Pointer to a user-provided list of Unicode range (2 value per range, values are inclusive,
	/// zero-terminated list). THE ARRAY DATA NEEDS TO PERSIST AS LONG AS THE FONT IS ALIVE.
	const font_wchar* glyph_ranges{};
	// Minimum AdvanceX for glyphs, set Min to align font icons, set both Min/Max to enforce mono-space font
	float glyph_min_advance_x{};
	/// Maximum AdvanceX for glyphs
	float glyph_max_advance_x{std::numeric_limits<float>::max()};
	/// Merge into previous ImFont, so you can combine multiple inputs font into one ImFont (e.g. ASCII font +
	/// icons + Japanese glyphs). You may want to use GlyphOffset.y when merge font of different heights.
	bool merge_mode{};
	/// Settings for custom font rasterizer. Leave as zero if you aren't using one.
	unsigned int rasterizer_flags{};
	/// Brighten (>1.0f) or darken (<1.0f) font output. Brightening small fonts may be a good workaround to
	/// make them more readable.
	float rasterizer_multiply{1.0f};

	/// Load kerning table if requested glyphs are below this value
	uint32_t kerning_glyphs_limit{};

	/// [Internal]
	font_info* dst_font{};
};

struct font_glyph
{
	/// 0x0000..0xFFFF
	font_wchar codepoint{};

	/// Distance to next character (= data from font + font_config::glyph_extra_spacing_x baked in)
	float advance_x{};

	/// Glyph corners
	float x0{}, y0{}, x1{}, y1{};

	/// Texture coordinates
	float u0{}, v0{}, u1{}, v1{};
};

// Helper to build glyph ranges from text/string data. Feed your application strings/characters to it then
// call build_ranges(). This is essentially a tightly packed of vector of 64k booleans = 8KB storage.
struct font_glyph_ranges_builder
{
	// Store 1-bit per Unicode code point (0=unused, 1=used)
	std::vector<uint32_t> used_chars;

	font_glyph_ranges_builder()
	{
		clear();
	}
	inline void clear()
	{
		size_t size_in_bytes = 0x10000 / 8;
		used_chars.resize(size_in_bytes / sizeof(uint32_t), 0);
	}
	// Get bit n in the array
	inline bool get_bit(int n) const
	{
		int off = (n >> 5);
		uint32_t mask = 1u << (n & 31);
		return (used_chars[size_t(off)] & mask) != 0;
	}

	// Set bit n in the array
	inline void set_bit(int n)
	{
		int off = (n >> 5);
		uint32_t mask = 1u << (n & 31);
		used_chars[size_t(off)] |= mask;
	}

	// Add character
	inline void add_char(font_wchar c)
	{
		set_bit(c);
	}

	// Add string (each character of the UTF-8 string are added)
	void add_text(const char* text, const char* text_end = nullptr);

	// Add ranges, e.g.
	// builder.add_ranges(fnt::get_glyph_ranges_default()) to force
	// add all of ASCII/Latin+Ext
	void add_ranges(const font_wchar* ranges);
	// Output new ranges
	std::vector<font_wchar> build_ranges();
};

//-------------------------------------------
// Glyph Ranges
//-------------------------------------------

// Helpers to retrieve list of common Unicode ranges (2 value per range, values are inclusive,
// zero-terminated list) NB: Make sure that your string are UTF-8 and NOT in your local code page. In
// C++11, you can create UTF-8 string literal using the u8"Hello world" syntax. See FAQ for details. NB:
// Consider using ImFontGlyphRangesBuilder to build glyph ranges from textual data.

const font_wchar* get_glyph_ranges_default();
// Basic Latin, Extended Latin
const font_wchar* get_glyph_ranges_latin();
// Default + Korean characters
const font_wchar* get_glyph_ranges_korean();
// Default + Hiragana, Katakana, Half-Width, Selection of 1946 Ideographs
const font_wchar* get_glyph_ranges_japanese();
// Default + Half-Width + Japanese Hiragana/Katakana + full set of about 21000 CJK Unified Ideographs
const font_wchar* get_glyph_ranges_chinese_full();

// Default + Half-Width + Japanese
// Hiragana/Katakana + set of 2500 CJK Unified
// Ideographs for common simplified Chinese
const font_wchar* get_glyph_ranges_chinese_simplified_common();

// Default + about 400 Cyrillic characters
const font_wchar* get_glyph_ranges_cyrillic();
// Default + Thai characters
const font_wchar* get_glyph_ranges_thai();
// Default + Vietname characters
const font_wchar* get_glyph_ranges_vietnamese();

int text_char_from_utf8(unsigned int* out_char, const char* in_text, const char* in_text_end);

// Load and rasterize multiple TTF/OTF fonts into a same texture. The font atlas will build a single texture
// holding:
//  - One or more fonts.
// It is the user-code responsibility to setup/build the atlas, then upload the pixel data into a texture
// accessible by your graphics api.
//  - Optionally, call any of the AddFont*** functions. If you don't call any, the default font embedded in
//  the code will be loaded for you.

// Common pitfalls:
// - If you pass a 'glyph_ranges' array to AddFont*** functions, you need to make sure that your array persist
// up until the
//   atlas is build (when calling build()). We only copy the pointer, not the data.
// - Important: By default, AddFontFromMemoryTTF() takes ownership of the data. Even though we are not writing
// to it, we will free the pointer on destruction.
//   You can set font_cfg->font_data_owned_by_atlas=false to keep ownership of your data and it won't be
//   freed,
// - Even though many functions are suffixed with "TTF", OTF data is supported just as well.
// - This is an old API and it is currently awkward for those and and various other reasons! We will address
// them in the future!
struct font_atlas
{
	~font_atlas();
	font_info* add_font(const font_config* font_cfg);
	font_info* add_font_default(const font_config* font_cfg = nullptr);
	font_info* add_font_from_file_ttf(const char* filename, float size_pixels,
									  const font_config* font_cfg = nullptr,
									  const font_wchar* glyph_ranges = nullptr);
	/// Note: Transfer ownership of 'ttf_data' to ImFontAtlas! Will
	/// be deleted after destruction of the atlas. Set
	/// font_cfg->FontDataOwnedByAtlas=false to keep ownership of
	/// your data and it won't be freed.
	font_info* add_font_from_memory_ttf(void* font_data, size_t font_size, float size_pixels,
										const font_config* font_cfg = nullptr,
										const font_wchar* glyph_ranges = nullptr);
	/// 'compressed_font_data' still
	/// owned by caller. Compress with
	/// binary_to_compressed_c.cpp.
	font_info* add_font_from_memory_compressed_ttf(const void* compressed_font_data,
												   size_t compressed_font_size, float size_pixels,
												   const font_config* font_cfg = nullptr,
												   const font_wchar* glyph_ranges = nullptr);

	/// 'compressed_font_data_base85' still owned by caller.
	/// Compress with binary_to_compressed_c.cpp with -base85
	/// parameter.
	font_info* add_font_from_memory_compressed_base85_ttf(const char* compressed_font_data_base85,
														  float size_pixels,
														  const font_config* font_cfg = nullptr,
														  const font_wchar* glyph_ranges = nullptr);
	/// Clear input data (all ImFontConfig structures including sizes, TTF data, glyph
	/// ranges, etc.) = all the data used to build the texture and fonts.
	void clear_input_data();

	/// Clear output texture data (CPU side). Saves RAM once the texture has been copied to graphics memory.
	void clear_tex_data();

	/// Clear output font data (glyphs storage, UV coordinates).
	void clear_fonts();

	/// Clear all input and output.
	void clear();

	/// Build atlas, retrieve pixel data.
	/// User is in charge of copying the pixels into graphics memory (e.g. create a texture with your engine).
	/// The pitch is always = Width * BytesPerPixels (1 or 4)
	/// Building in RGBA32 format is provided for convenience and compatibility
	bool build(font_rasterizer raster, std::string& err);
	void get_tex_data_as_alpha8(uint8_t** out_pixels, uint32_t* out_width, uint32_t* out_height,
								uint32_t* out_bytes_per_pixel = nullptr); // 1 byte per-pixel
	void get_tex_data_as_rgba32(uint8_t** out_pixels, uint32_t* out_width, uint32_t* out_height,
								uint32_t* out_bytes_per_pixel = nullptr); // 4 bytes-per-pixel

	bool is_built()
	{
		return !fonts.empty() && (!tex_pixels_alpha8.empty() || !tex_pixels_rgba32.empty());
	}

	void finish();
	void setup_font(font_info* font, font_config* font_config, float ascent, float descent, float line_heigt);
	//-------------------------------------------
	// Members
	//-------------------------------------------

	// 1 component per pixel, each component is unsigned 8-bit. Total size =
	// tex_width * tex_height
	std::vector<uint8_t> tex_pixels_alpha8{};
	// 4 component per pixel, each component is unsigned 8-bit. Total size =
	// TexWidth * tex_height * 4
	std::vector<uint32_t> tex_pixels_rgba32{};

	// Hold all the fonts returned by add_font*.
	std::vector<std::unique_ptr<font_info>> fonts{};
	// Internal data
	std::vector<font_config> config_data{};

	// Build flags
	font_atlas_flags flags{font_atlas_flags::none};

	// Max texture size allowed. Must be a power of 2
	uint32_t max_texture_size{4096};
	// Padding between glyphs within texture in pixels. Defaults to 1. If your rendering
	// method doesn't rely on bilinear filtering you may set this to 0.
	uint32_t tex_glyph_padding{1};

	uint32_t sdf_spread{};
	// [Internal]
	// NB: Access texture data via get_tex_data*() calls! Which will setup a default font for you.
	// Texture width calculated during build().
	uint32_t tex_width{};
	// Texture height calculated during build().
	uint32_t tex_height{};
};

// Font runtime data and rendering
struct font_info
{
	kerning_table kernings{};

	// Sparse. glyphs->AdvanceX in a directly
	std::vector<float> index_advance_x{}; // out

	// = FallbackGlyph->AdvanceX
	float fallback_advance_x{}; // out

	// Height of characters/line, set during loading (don't change after loading)
	float font_size{}; // in

	// Sparse. Index glyphs by Unicode code-point.
	std::vector<font_wchar> index_lookup{}; // out
	// All glyphs.
	std::vector<font_glyph> glyphs{}; // out

	// = FindGlyph(FontFallbackChar)
	const font_glyph* fallback_glyph{}; // out

	// What we has been loaded into
	font_atlas* container_atlas{}; // out //

	// Pointer within ContainerAtlas->ConfigData
	const font_config* config_data{}; // in

	// Number of ImFontConfig involved in creating this font. Bigger than 1
	// when merging multiple font sources into one ImFont.
	short config_data_count{}; // in

	// Replacement glyph if one isn't found. Only set via SetFallbackChar()
	font_wchar fallback_char{'?'}; // in

	// Base font scale
	float scale{1.0f}; // in

	// Ascent: distance from top to bottom of e.g. 'A' [0..FontSize]
	float ascent{};  // out
	float descent{}; // out
	float line_height{};

	// Total surface in pixels to get an idea of the
	// font rasterization/texture cost (not exact, we approximate the cost of padding
	// between glyphs)
	int metrics_total_surface{}; // out

	bool dirty_lookup_tables{}; // out

	// Methods
	~font_info();

	const font_glyph* find_glyph(font_wchar c) const;
	const font_glyph* find_glyph_no_fallback(font_wchar c) const;
	float get_char_advance(font_wchar c) const
	{
		return (size_t(c) < index_advance_x.size()) ? index_advance_x[size_t(c)] : fallback_advance_x;
	}
	bool is_loaded() const
	{
		return container_atlas != nullptr;
	}

	// [Internal] Don't use!
	void build_lookup_table();
	void clear_output_data();
	void grow_index(size_t new_size);
	void add_glyph(font_wchar c, float x0, float y0, float x1, float y1, float u0, float v0, float u1,
				   float v1, float advance_x);

	// Makes 'dst' character/glyph
	// points to 'src'
	// character/glyph. Currently
	// needs to be called AFTER fonts
	// have been built.
	void add_remap_char(font_wchar dst, font_wchar src, bool overwrite_dst = true);
	void set_fallback_char(font_wchar c);
};

} // namespace
