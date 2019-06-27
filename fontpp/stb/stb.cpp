#include "stb.h"
#include "../font.h"
#include <limits>

#define STB_RECT_PACK_IMPLEMENTATION
#include "stb_rectpack.h"

#define STB_TRUETYPE_IMPLEMENTATION
#define STBTT_STATIC
#include "stb_truetype.h"

namespace fnt
{
namespace stb
{
namespace
{
void font_atlas_build_multiply_calc_lookup_table(uint8_t out_table[256], float in_brighten_factor)
{
	for(unsigned int i = 0; i < 256; i++)
	{
		auto value = static_cast<unsigned int>(i * in_brighten_factor);
		out_table[i] = value > 255 ? 255 : (value & 0xFF);
	}
}

void font_atlas_build_multiply_rect_alpha8(const uint8_t table[256], uint8_t* pixels, int x, int y, int w,
										   int h, int stride)
{
	uint8_t* data = pixels + x + y * stride;
	for(int j = h; j > 0; j--, data += stride)
		for(int i = 0; i < w; i++)
			data[i] = table[data[i]];
}

// Temporary data for one source font (multiple source fonts can be merged into one destination font_info)
// (C++03 doesn't allow instancing std::vector<> with function-local types so we declare the type here.)
struct font_info_build_src_data
{
	stbtt_fontinfo font_info{};
	// Hold the list of codepoints to pack (essentially points to Codepoints.Data)
	stbtt_pack_range pack_range{};
	// Rectangle to pack. We first fill in their size and the packer will give us their position.
	stbrp_rect* rects{};
	// Output glyphs
	stbtt_packedchar* packed_chars{};
	// Ranges as requested by user (user is allowed to request too much, e.g. 0x0020..0xFFFF)
	const font_wchar* src_ranges{};
	// Index into atlas->fonts[] and dst_tmp_array[]
	int dst_index{};
	// Highest requested codepoint
	int glyphs_highest{};
	// Glyph count (excluding missing glyphs and glyphs already set by an earlier source font)
	int glyphs_count{};
	// Glyph bit map (random access, 1-bit per codepoint. This will be a maximum of 8KB)
	bool_vector glyphs_set{};
	// Glyph codepoints list (flattened version of GlyphsMap)
	std::vector<int> glyphs_list{};
};

// Temporary data for one destination font_info* (multiple source fonts can be merged into one destination
// font_info)
struct font_info_build_dst_data
{
	// Number of source fonts targeting this destination font.
	int src_count{};
	int glyphs_highest{};
	int glyphs_count{};
	// This is used to resolve collision when multiple sources are merged into a same destination font.
	bool_vector glyphs_set{};
};

void unpack_bool_vector_to_flat_index_list(const bool_vector* in, std::vector<int>* out)
{
	assert(sizeof(in->storage[0]) == sizeof(int));
	int idx = 0;
	for(auto entries_32 : in->storage)
	{
		for(int bit_n = 0; bit_n < 32; bit_n++)
		{
			if(uint32_t(entries_32) & (1u << bit_n))
			{
				out->push_back((idx << 5) + bit_n);
			}
		}
		idx++;
	}
}
}

bool build(font_atlas* atlas, std::string& err)
{
	assert(!atlas->config_data.empty());
	// Clear atlas
	atlas->tex_width = atlas->tex_height = 0;
	atlas->clear_tex_data();

	// Temporary storage for building
	std::vector<font_info_build_src_data> src_tmp_array;
	std::vector<font_info_build_dst_data> dst_tmp_array;
	src_tmp_array.resize(atlas->config_data.size());
	dst_tmp_array.resize(atlas->fonts.size());

	// 1. Initialize font loading structure, check font data validity
	for(size_t src_i = 0; src_i < atlas->config_data.size(); src_i++)
	{
		auto& src_tmp = src_tmp_array[src_i];
		auto& cfg = atlas->config_data[src_i];
		assert(cfg.dst_font && (!cfg.dst_font->is_loaded() || cfg.dst_font->container_atlas == atlas));

		// Find index from cfg.DstFont (we allow the user to set cfg.DstFont. Also it makes casual debugging
		// nicer than when storing indices)
		src_tmp.dst_index = -1;
		for(size_t output_i = 0; output_i < atlas->fonts.size() && src_tmp.dst_index == -1; output_i++)
			if(cfg.dst_font == atlas->fonts[output_i].get())
				src_tmp.dst_index = static_cast<int>(output_i);
		assert(src_tmp.dst_index != -1); // cfg.DstFont not pointing within atlas->fonts[] array?
		if(src_tmp.dst_index == -1)
			return false;

		// Initialize helper structure for font loading and verify that the TTF/OTF data is correct
		const int font_offset =
			stbtt_GetFontOffsetForIndex(reinterpret_cast<const uint8_t*>(cfg.font_data), cfg.font_no);
		assert(font_offset >= 0 && "FontData is incorrect, or FontNo cannot be found.");
		if(!stbtt_InitFont(&src_tmp.font_info, reinterpret_cast<const uint8_t*>(cfg.font_data), font_offset))
			return false;

		// Measure highest codepoints
		auto& dst_tmp = dst_tmp_array[size_t(src_tmp.dst_index)];
		src_tmp.src_ranges = cfg.glyph_ranges ? cfg.glyph_ranges : get_glyph_ranges_default();
		for(const font_wchar* src_range = src_tmp.src_ranges; src_range[0] && src_range[1]; src_range += 2)
			src_tmp.glyphs_highest = std::max(src_tmp.glyphs_highest, int(src_range[1]));
		dst_tmp.src_count++;
		dst_tmp.glyphs_highest = std::max(dst_tmp.glyphs_highest, src_tmp.glyphs_highest);
	}

	// 2. For every requested codepoint, check for their presence in the font data, and handle redundancy or
	// overlaps between source fonts to avoid unused glyphs.
	size_t total_glyphs_count = 0;
	for(auto& src_tmp : src_tmp_array)
	{
		auto& dst_tmp = dst_tmp_array[size_t(src_tmp.dst_index)];
		src_tmp.glyphs_set.resize(src_tmp.glyphs_highest + 1);
		if(dst_tmp.glyphs_set.storage.empty())
			dst_tmp.glyphs_set.resize(dst_tmp.glyphs_highest + 1);

		for(const font_wchar* src_range = src_tmp.src_ranges; src_range[0] && src_range[1]; src_range += 2)
			for(int codepoint = src_range[0]; codepoint <= src_range[1]; codepoint++)
			{
				if(dst_tmp.glyphs_set.get_bit(
					   codepoint)) // Don't overwrite existing glyphs. We could make this
								   // an option for MergeMode (e.g. MergeOverwrite==true)
					continue;
				if(!stbtt_FindGlyphIndex(&src_tmp.font_info, codepoint)) // It is actually in the font?
					continue;

				// Add to avail set/counters
				src_tmp.glyphs_count++;
				dst_tmp.glyphs_count++;
				src_tmp.glyphs_set.set_bit(codepoint, true);
				dst_tmp.glyphs_set.set_bit(codepoint, true);
				total_glyphs_count++;
			}
	}

	// 3. Unpack our bit map into a flat list (we now have all the Unicode points that we know are requested
	// _and_ available _and_ not overlapping another)
	for(auto& src_tmp : src_tmp_array)
	{
		src_tmp.glyphs_list.reserve(size_t(src_tmp.glyphs_count));
		unpack_bool_vector_to_flat_index_list(&src_tmp.glyphs_set, &src_tmp.glyphs_list);
		src_tmp.glyphs_set.clear();
		assert(src_tmp.glyphs_list.size() == size_t(src_tmp.glyphs_count));
	}
	for(auto& dst_i : dst_tmp_array)
		dst_i.glyphs_set.clear();
	dst_tmp_array.clear();

	// Allocate packing character data and flag packed characters buffer as non-packed (x0=y0=x1=y1=0)
	// (We technically don't need to zero-clear buf_rects, but let's do it for the sake of sanity)
	std::vector<stbrp_rect> buf_rects;
	std::vector<stbtt_packedchar> buf_packedchars;
	buf_rects.resize(total_glyphs_count);
	buf_packedchars.resize(total_glyphs_count);

	// 4. Gather glyphs sizes so we can pack them in our virtual canvas.
	size_t total_surface = 0;
	size_t buf_rects_out_n = 0;
	size_t buf_packedchars_out_n = 0;
	for(size_t src_i = 0; src_i < src_tmp_array.size(); src_i++)
	{
		auto& src_tmp = src_tmp_array[src_i];
		if(src_tmp.glyphs_count == 0)
			continue;

		src_tmp.rects = &buf_rects[buf_rects_out_n];
		src_tmp.packed_chars = &buf_packedchars[buf_packedchars_out_n];
		buf_rects_out_n += size_t(src_tmp.glyphs_count);
		buf_packedchars_out_n += size_t(src_tmp.glyphs_count);

		// Convert our ranges in the format stb_truetype wants
		auto& cfg = atlas->config_data[src_i];
		src_tmp.pack_range.font_size = cfg.size_pixels;
		src_tmp.pack_range.first_unicode_codepoint_in_range = 0;
		src_tmp.pack_range.array_of_unicode_codepoints = src_tmp.glyphs_list.data();
		src_tmp.pack_range.num_chars = static_cast<int>(src_tmp.glyphs_list.size());
		src_tmp.pack_range.chardata_for_range = src_tmp.packed_chars;
		src_tmp.pack_range.h_oversample = static_cast<uint8_t>(cfg.oversample_h);
		src_tmp.pack_range.v_oversample = static_cast<uint8_t>(cfg.oversample_v);

		// Gather the sizes of all rectangles we will need to pack (this loop is based on
		// stbtt_PackFontRangesGatherRects)
		const float scale = (cfg.size_pixels > 0)
								? stbtt_ScaleForPixelHeight(&src_tmp.font_info, cfg.size_pixels)
								: stbtt_ScaleForMappingEmToPixels(&src_tmp.font_info, -cfg.size_pixels);
		const auto padding = int(atlas->tex_glyph_padding);
		for(size_t glyph_i = 0; glyph_i < src_tmp.glyphs_list.size(); glyph_i++)
		{
			int x0, y0, x1, y1;
			const int glyph_index_in_font =
				stbtt_FindGlyphIndex(&src_tmp.font_info, src_tmp.glyphs_list[glyph_i]);
			assert(glyph_index_in_font != 0);
			stbtt_GetGlyphBitmapBoxSubpixel(&src_tmp.font_info, glyph_index_in_font, scale * cfg.oversample_h,
											scale * cfg.oversample_v, 0, 0, &x0, &y0, &x1, &y1);
			src_tmp.rects[glyph_i].w = stbrp_coord(x1 - x0 + padding + cfg.oversample_h - 1);
			src_tmp.rects[glyph_i].h = stbrp_coord(y1 - y0 + padding + cfg.oversample_v - 1);
			total_surface += src_tmp.rects[glyph_i].w * src_tmp.rects[glyph_i].h;
		}
	}
	const uint32_t tex_max = atlas->max_texture_size;

	// We need a width for the skyline algorithm, any width!
	// The exact width doesn't really matter much, but some API/GPU have texture size limitations and
	// increasing width can decrease height. User can override TexDesiredWidth and TexGlyphPadding if they
	// wish, otherwise we use a simple heuristic to select the width based on expected surface.
	const auto surface_sqrt = uint32_t(std::sqrt(total_surface)) + 1;
	atlas->tex_height = 0;
	atlas->tex_width = estimate_width(tex_max, 256, surface_sqrt);

	// 5. Start packing
	// Pack our extra data rectangles first, so it will be on the upper-left corner of our texture (UV will
	// have small values).
	stbtt_pack_context spc = {};
	stbtt_PackBegin(&spc, nullptr, int(atlas->tex_width), int(1024 * 32), 0, int(atlas->tex_glyph_padding),
					nullptr);

	// 6. Pack each source font. No rendering yet, we are working with rectangles in an infinitely tall
	// texture at this point.
	for(auto& src_tmp : src_tmp_array)
	{
		if(src_tmp.glyphs_count == 0)
			continue;

		stbrp_pack_rects(reinterpret_cast<stbrp_context*>(spc.pack_info), src_tmp.rects,
						 src_tmp.glyphs_count);

		// Extend texture height and mark missing glyphs as non-packed so we won't render them.
		// FIXME: We are not handling packing failure here (would happen if we got off tex_height_max or if a
		// single if larger than TexWidth?)
		for(int glyph_i = 0; glyph_i < src_tmp.glyphs_count; glyph_i++)
			if(src_tmp.rects[glyph_i].was_packed)
				atlas->tex_height = std::max<uint32_t>(atlas->tex_height,
													   src_tmp.rects[glyph_i].y + src_tmp.rects[glyph_i].h);
	}

	// 7. Allocate texture
	atlas->tex_height = (atlas->flags & font_atlas_flags::no_power_of_two_height)
							? (atlas->tex_height + 1)
							: upper_power_of_two(atlas->tex_height);

	if(atlas->tex_width == 0 || atlas->tex_height == 0)
	{
		err = "Could not allocate texture.";
		return false;
	}

	if(atlas->tex_height > tex_max)
	{
		err = "Required [" + std::to_string(atlas->tex_height) +
			  "] size is too big. Consider creating the font with a lower size and "
			  "upscaling it when displaying.";
		return false;
	}

	atlas->tex_pixels_alpha8.resize(atlas->tex_width * atlas->tex_height, 0);
	spc.pixels = atlas->tex_pixels_alpha8.data();
	spc.height = int(atlas->tex_height);

	// 8. Render/rasterize font characters into the texture
	for(size_t src_i = 0; src_i < src_tmp_array.size(); src_i++)
	{
		auto& cfg = atlas->config_data[src_i];
		auto& src_tmp = src_tmp_array[src_i];
		if(src_tmp.glyphs_count == 0)
			continue;

		auto status = stbtt_PackFontRangesRenderIntoRects(&spc, &src_tmp.font_info, &src_tmp.pack_range, 1,
														  src_tmp.rects);

		if(status == 0)
		{
			err = "Required [" + std::to_string(src_tmp.glyphs_count) +
				  "] glyphs are too many. Consider creating the font with a lower size and "
				  "upscaling it when displaying.";
			return false;
		}
		// Apply multiply operator
		if(cfg.rasterizer_multiply != 1.0f)
		{
			uint8_t multiply_table[256];
			font_atlas_build_multiply_calc_lookup_table(multiply_table, cfg.rasterizer_multiply);
			stbrp_rect* r = &src_tmp.rects[0];
			for(int glyph_i = 0; glyph_i < src_tmp.glyphs_count; glyph_i++, r++)
				if(r->was_packed)
					font_atlas_build_multiply_rect_alpha8(multiply_table, atlas->tex_pixels_alpha8.data(),
														  r->x, r->y, r->w, r->h, int(atlas->tex_width * 1));
		}
		src_tmp.rects = nullptr;
	}

	// End packing
	stbtt_PackEnd(&spc);
	buf_rects.clear();

	// 9. Setup font_info and glyphs for runtime
	for(size_t src_i = 0; src_i < src_tmp_array.size(); src_i++)
	{
		auto& src_tmp = src_tmp_array[src_i];
		if(src_tmp.glyphs_count == 0)
			continue;

		auto& cfg = atlas->config_data[src_i];
		// We can have multiple input fonts writing into a same destination
		// font (when using MergeMode=true)
		auto& dst_font = cfg.dst_font;

		const float font_scale = stbtt_ScaleForPixelHeight(&src_tmp.font_info, cfg.size_pixels);
		int unscaled_ascent, unscaled_descent, unscaled_line_gap;
		stbtt_GetFontVMetrics(&src_tmp.font_info, &unscaled_ascent, &unscaled_descent, &unscaled_line_gap);

		const float ascent = std::floor(unscaled_ascent * font_scale + ((unscaled_ascent > 0.0f) ? +1 : -1));
		const float descent =
			std::floor(unscaled_descent * font_scale + ((unscaled_descent > 0.0f) ? +1 : -1));
		const float line_height = (ascent - descent) + unscaled_line_gap * font_scale;
		atlas->setup_font(dst_font, &cfg, ascent, descent, line_height);
		const float font_off_x = cfg.glyph_offset_x;
		const float font_off_y = cfg.glyph_offset_y;
		const auto sdf_spread = atlas->sdf_spread;
		bool has_kerning_table = src_tmp.font_info.kern || src_tmp.font_info.gpos;

		for(int glyph_i = 0; glyph_i < src_tmp.glyphs_count; glyph_i++)
		{
			const int codepoint = src_tmp.glyphs_list[size_t(glyph_i)];
			const auto& pc = src_tmp.packed_chars[size_t(glyph_i)];

			const float char_advance_x_org = pc.xadvance;
			const float char_advance_x_mod =
				clamp(char_advance_x_org, cfg.glyph_min_advance_x, cfg.glyph_max_advance_x);
			float char_off_x = font_off_x;
			if(fabs(char_advance_x_org - char_advance_x_mod) > std::numeric_limits<float>::epsilon())
				char_off_x += cfg.pixel_snap_h ? static_cast<float>(static_cast<int>(
													 (char_advance_x_mod - char_advance_x_org) * 0.5f))
											   : (char_advance_x_mod - char_advance_x_org) * 0.5f;

			// Register glyph
			stbtt_aligned_quad q;
			float dummy_x = 0.0f, dummy_y = 0.0f;
			stbtt_GetPackedQuad(src_tmp.packed_chars, int(atlas->tex_width), int(atlas->tex_height), glyph_i,
								&dummy_x, &dummy_y, &q, 0);

			float xsize = q.s1 - q.s0;
			float ysize = q.t1 - q.t0;
			auto sdf_shift_x = (float(sdf_spread)) / atlas->tex_width;
			auto sdf_shift_y = (float(sdf_spread)) / atlas->tex_height;
			auto u0 = q.s0 - sdf_shift_x;
			auto v0 = q.t0 - sdf_shift_y;
			auto u1 = q.s1 + sdf_shift_x;
			auto v1 = q.t1 + sdf_shift_y;

			if(xsize > 0.0f)
			{
				sdf_shift_x = sdf_shift_x / xsize;
			}
			if(ysize > 0.0f)
			{
				sdf_shift_y = sdf_shift_y / ysize;
			}
			xsize = q.x1 - q.x0;
			ysize = q.y1 - q.y0;
			sdf_shift_x *= xsize;
			sdf_shift_y *= ysize;

			auto x0 = q.x0 - sdf_shift_x;
			auto y0 = q.y0 - sdf_shift_y;
			auto x1 = q.x1 + sdf_shift_x;
			auto y1 = q.y1 + sdf_shift_y;

			// if no kerning table, don't waste time looking
			if(has_kerning_table && cfg.kerning_glyphs_limit > uint32_t(src_tmp.glyphs_count))
			{
				for(int glyph_j = 0; glyph_j < src_tmp.glyphs_count; glyph_j++)
				{
					const int codepoint_from = src_tmp.glyphs_list[size_t(glyph_j)];
					auto kerning =
						stbtt_GetCodepointKernAdvance(&src_tmp.font_info, codepoint_from, codepoint);
					if(kerning)
					{
						auto cp_from = font_wchar(codepoint_from);
						auto cp_to = font_wchar(codepoint);

						auto kern_value = float(kerning) * font_scale;
						dst_font->kernings[{cp_from, cp_to}] = kern_value;
					}
				}
			}

			dst_font->add_glyph(font_wchar(codepoint), x0 + char_off_x, y0 + font_off_y, x1 + char_off_x,
								y1 + font_off_y, u0, v0, u1, v1, char_advance_x_mod);
		}
	}

	atlas->finish();
	return true;
}
}
} // namespace library_template
