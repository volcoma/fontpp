// Wrapper to use FreeType (instead of stb_truetype) for Dear ImGui
// Get latest version at https://github.com/ocornut/imgui/tree/master/misc/freetype
// Original code by @vuhdo (Aleksei Skriabin). Improvements by @mikesart. Maintained and v0.60+ by @ocornut.

// Changelog:
// - v0.50: (2017/08/16) imported from https://github.com/Vuhdo/imgui_freetype into
// http://www.github.com/ocornut/imgui_club, updated for latest changes in ImFontAtlas, minor tweaks.
// - v0.51: (2017/08/26) cleanup, optimizations, support for font_config::RasterizerFlags,
// font_config::RasterizerMultiply.
// - v0.52: (2017/09/26) fixes for imgui internal changes.
// - v0.53: (2017/10/22) minor inconsequential change to match change in master (removed an unnecessary
// statement).
// - v0.54: (2018/01/22) fix for addition of ImFontAtlas::TexUvscale member.
// - v0.55: (2018/02/04) moved to main imgui repository (away from http://www.github.com/ocornut/imgui_club)
// - v0.56: (2018/06/08) added support for font_config::GlyphMinAdvanceX, GlyphMaxAdvanceX.
// - v0.60: (2019/01/10) re-factored to match big update in STB builder. fixed texture height waste. fixed
// redundant glyphs when merging. support for glyph padding.
// - v0.61: (2019/01/15) added support for imgui allocators + added FreeType only override function
// SetAllocatorFunctions().
// - v0.62: (2019/02/09) added RasterizerFlags::Monochrome flag to disable font anti-aliasing (combine with
// ::MonoHinting for best results!)

// Gamma Correct Blending:
//  FreeType assumes blending in linear space rather than gamma space.
//  See https://www.freetype.org/freetype2/docs/reference/ft2-base_interface.html#FT_Render_Glyph
//  For correct results you need to be using sRGB and convert to linear space in the pixel shader output.
//  The default imgui styles will be impacted by this change (alpha values will need tweaking).

// FIXME: cfg.OversampleH, OversampleV are not supported (but perhaps not so necessary with this rasterizer).

#include "freetype.h"
#include "../font.h"

#include <ft2build.h>
#include <freetype/tttables.h>
#include <assert.h>
#include <cstring>
#include <cmath>

#include FT_FREETYPE_H  // <freetype/freetype.h>
#include FT_GLYPH_H     // <freetype/ftglyph.h>
#include FT_MODULE_H    // <freetype/ftmodapi.h>
#include FT_SYNTHESIS_H // <freetype/ftsynth.h>

//const char* get_ft_error_str(FT_Error err)
//{
//    #undef __FTERRORS_H__
//    #define FT_ERRORDEF( e, v, s )  case e: return s;
//    #define FT_ERROR_START_LIST     switch (err) {
//    #define FT_ERROR_END_LIST       }
//    #include FT_ERRORS_H
//    return "(Unknown error)";
//}
#ifdef _MSC_VER
#pragma warning(disable : 4505) // unreferenced local function has been removed (stb stuff)
#endif

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wunused-function" // warning: 'xxxx' defined but not used
#endif

#ifndef STB_RECT_PACK_IMPLEMENTATION // in case the user already have an implementation in the _same_
    // compilation unit (e.g. unity builds)
#define STBRP_ASSERT(x) assert(x)
#define STBRP_STATIC
#define STB_RECT_PACK_IMPLEMENTATION
#include "../stb/stb_rectpack.h"
#endif

namespace fnt
{
namespace freetype
{

namespace
{
// Glyph metrics:
// --------------
//
//                       xmin                     xmax
//                        |                         |
//                        |<-------- width -------->|
//                        |                         |
//              |         +-------------------------+----------------- ymax
//              |         |    ggggggggg   ggggg    |     ^        ^
//              |         |   g:::::::::ggg::::g    |     |        |
//              |         |  g:::::::::::::::::g    |     |        |
//              |         | g::::::ggggg::::::gg    |     |        |
//              |         | g:::::g     g:::::g     |     |        |
//    offsetX  -|-------->| g:::::g     g:::::g     |  offsetY     |
//              |         | g:::::g     g:::::g     |     |        |
//              |         | g::::::g    g:::::g     |     |        |
//              |         | g:::::::ggggg:::::g     |     |        |
//              |         |  g::::::::::::::::g     |     |      height
//              |         |   gg::::::::::::::g     |     |        |
//  baseline ---*---------|---- gggggggg::::::g-----*--------      |
//            / |         |             g:::::g     |              |
//     origin   |         | gggggg      g:::::g     |              |
//              |         | g:::::gg   gg:::::g     |              |
//              |         |  g::::::ggg:::::::g     |              |
//              |         |   gg:::::::::::::g      |              |
//              |         |     ggg::::::ggg        |              |
//              |         |         gggggg          |              v
//              |         +-------------------------+----------------- ymin
//              |                                   |
//              |------------- advanceX ----------->|

/// A structure that describe a glyph.
struct glyph_info_ft
{
    // Glyph's width in pixels.
    uint32_t width{};
    // Glyph's height in pixels.
    uint32_t height{};
    // The distance from the origin ("pen position") to the left of the glyph.
    FT_Int offset_x{};
    // The distance from the origin to the top of the glyph. This is usually a value < 0.
    FT_Int offset_y{};
    // The distance from the origin to the origin of the next glyph. This is usually a value > 0.
    float advance_x{};
};

// Font parameters and metrics.
struct font_info_ft
{
    // Size this font was generated with.
    uint32_t pixel_height{};
    // The pixel extents above the baseline in pixels (typically positive).
    float ascender{};
    // The extents below the baseline in pixels (typically negative).
    float descender{};
    // The baseline-to-baseline distance. Note that it usually is larger than the sum of
    // the ascender and descender taken as absolute values. There is also no guarantee that
    // no glyphs extend above or below subsequent baselines when using this distance. Think
    // of it as a value the designer of the font finds appropriate.
    float line_spacing{};
    // The spacing in pixels between one row's descent and the next row's ascent.
    float line_gap{};
    // This field gives the maximum horizontal cursor advance for all glyphs in the font.
    float max_advance_width{};
    float xheight{};
    float cap_height{};
    float ysuperscript_offset{};
    float ysuperscript_size{};
    float ysubscript_offset{};
    float ysubscript_size{};
};


struct glyph_ft
{
    glyph_info_ft info{};
    uint32_t codepoint{};
    uint32_t glyph_index{};
    // Point within one of the dst_tmp_bitmap_buffers[] array
    uint8_t* bitmap_data{};
};
// FreeType glyph rasterizer.
struct font_ft
{
    // Initialize from an external data buffer. Doesn't copy
    // data, and you must ensure it stays valid up to this
    // object lifetime.
    bool init(FT_Library ft_library, const font_config& cfg, unsigned int extra_user_flags);
    void close();
    // Change font pixel size. All following calls to RasterizeGlyph()
    // will use this size
    void set_pixel_height(int pixel_height);
    const FT_Glyph_Metrics* load_glyph(glyph_ft& in_glyph);
    const FT_Bitmap* render_glyph_and_get_info(glyph_info_ft* out_glyph_info);
    void blit_glyph(const FT_Bitmap* ft_bitmap, uint8_t* dst, uint32_t dst_pitch,
                    const uint8_t* multiply_table = nullptr);
    font_ft() = default;
    font_ft(const font_ft& rhs) = delete;
    font_ft& operator=(const font_ft& rhs) = delete;

    font_ft(font_ft&& rhs) noexcept
    {
        info = rhs.info;
        face = rhs.face;
        rhs.face = nullptr;
        user_flags = rhs.user_flags;
        load_flags = rhs.load_flags;
        render_mode = rhs.render_mode;
    }

    font_ft& operator=(font_ft&& rhs) noexcept
    {
        info = rhs.info;
        face = rhs.face;
        rhs.face = nullptr;
        user_flags = rhs.user_flags;
        load_flags = rhs.load_flags;
        render_mode = rhs.render_mode;

        return *this;
    }
    ~font_ft()
    {
        close();
    }
    // [Internals]
    // Font descriptor of the current font.
    font_info_ft info{};
    FT_Face face{};
    unsigned int user_flags{};
    FT_Int32 load_flags{};
    FT_Render_Mode render_mode{};
};

// Handy routines for converting from fixed point
#define FT_CEIL(X) ((((X) + 63) & -64) / 64.0f)

bool font_ft::init(FT_Library ft_library, const font_config& cfg, unsigned int extra_user_flags)
{
    FT_Error error = FT_New_Memory_Face(ft_library, (uint8_t*)cfg.font_data, (uint32_t)cfg.font_data_size,
                                        (uint32_t)cfg.font_no, &face);
    if(error != 0)
        return false;
    error = FT_Select_Charmap(face, FT_ENCODING_UNICODE);
    if(error != 0)
        return false;
    info = {};
    set_pixel_height((uint32_t)cfg.size_pixels);

    // Convert to FreeType flags (NB: Bold and Oblique are processed separately)
    user_flags = cfg.rasterizer_flags | extra_user_flags;
    load_flags = FT_LOAD_NO_BITMAP;
    if(user_flags & rasterizer_flags::no_hinting)
        load_flags |= FT_LOAD_NO_HINTING;
    if(user_flags & rasterizer_flags::no_auto_hint)
        load_flags |= FT_LOAD_NO_AUTOHINT;
    if(user_flags & rasterizer_flags::force_auto_hint)
        load_flags |= FT_LOAD_FORCE_AUTOHINT;
    if(user_flags & rasterizer_flags::light_hinting)
        load_flags |= FT_LOAD_TARGET_LIGHT;
    else if(user_flags & rasterizer_flags::mono_hinting)
        load_flags |= FT_LOAD_TARGET_MONO;
    else
        load_flags |= FT_LOAD_TARGET_NORMAL;

    if(user_flags & rasterizer_flags::monochrome)
        render_mode = FT_RENDER_MODE_MONO;
    else
        render_mode = FT_RENDER_MODE_NORMAL;

    return true;
}

void font_ft::close()
{
    if(face)
    {
        FT_Done_Face(face);
        face = nullptr;
    }
}

void font_ft::set_pixel_height(int pixel_height)
{
    // Vuhdo: I'm not sure how to deal with font sizes properly. As far as I understand, currently stb
    // assumes that the 'pixel_height' is a maximum height of an any given glyph, i.e. it's the sum of font's
    // ascender and descender. Seems strange to me. NB: FT_Set_Pixel_Sizes() doesn't seem to get us the same
    // result.

    FT_Size_RequestRec req;
    req.type = FT_SIZE_REQUEST_TYPE_REAL_DIM;
    req.width = 0;
    req.height = pixel_height * 64;
    req.horiResolution = 0;
    req.vertResolution = 0;
    FT_Request_Size(face, &req);

    // Update font info
    FT_Size_Metrics metrics = face->size->metrics;
    info.pixel_height = uint32_t(pixel_height);
    auto yscale = metrics.y_scale;
    auto xscale = metrics.x_scale;

    if(FT_IS_SCALABLE(face))
    {

        info.ascender = FT_CEIL(FT_MulFix(face->ascender, yscale));
        info.descender = FT_CEIL(FT_MulFix(face->descender, yscale));
        info.line_spacing = FT_CEIL(FT_MulFix(face->height, yscale));
        info.max_advance_width = FT_CEIL(FT_MulFix(face->max_advance_width, xscale));

    }
    else
    {
        info.ascender = FT_CEIL(metrics.ascender);
        info.descender = FT_CEIL(metrics.descender);
        info.line_spacing = FT_CEIL(metrics.height);
        info.max_advance_width = FT_CEIL(metrics.max_advance);
    }
    info.line_gap = info.line_spacing - info.ascender +info.descender;

    {
        auto table = reinterpret_cast<TT_OS2*>(FT_Get_Sfnt_Table(face, FT_Sfnt_Tag::FT_SFNT_OS2));
        if(table)
        {
            if(FT_IS_SCALABLE(face))
            {
                info.ysuperscript_size = FT_CEIL(FT_MulFix(table->ySuperscriptYSize, yscale));
                info.ysuperscript_offset = FT_CEIL(FT_MulFix(table->ySuperscriptYOffset, yscale));
                info.ysubscript_size = FT_CEIL(FT_MulFix(table->ySubscriptYSize, yscale));
                info.ysubscript_offset = FT_CEIL(FT_MulFix(table->ySubscriptYOffset, yscale));

                if(table->version >= 2)
                {
                    info.cap_height = FT_CEIL(FT_MulFix(table->sCapHeight, yscale));
                    info.xheight = FT_CEIL(FT_MulFix(table->sxHeight, yscale));
                }
            }
            else
            {
                info.ysuperscript_size = FT_CEIL(table->ySuperscriptYSize);
                info.ysuperscript_offset = FT_CEIL(table->ySuperscriptYOffset);
                info.ysubscript_size = FT_CEIL(table->ySubscriptYSize);
                info.ysubscript_offset = FT_CEIL(table->ySubscriptYOffset);

                if(table->version >= 2)
                {
                    info.cap_height = FT_CEIL(table->sCapHeight);
                    info.xheight = FT_CEIL(table->sxHeight);
                }
            }
        }
    }

    //    std::string family_name = face->family_name;
    //    std::string style_name = face->style_name;
    if(info.cap_height == 0.0f)
    {
        info.cap_height = info.ascender;

        for(auto codepoint : {'H', 'I'})
        {
            glyph_ft glyph{};
            glyph.codepoint = uint32_t(codepoint);
            auto glyph_metrics = load_glyph(glyph);
            if(glyph_metrics)
            {
                info.cap_height = FT_CEIL(glyph_metrics->height);
                break;
            }
        }
    }

    if(info.xheight == 0.0f)
    {
        info.xheight = info.cap_height * 0.5f;
        for(auto codepoint : {'x', 'z'})
        {
            glyph_ft glyph{};
            glyph.codepoint = uint32_t(codepoint);
            auto glyph_metrics = load_glyph(glyph);
            if(glyph_metrics)
            {
                info.xheight = FT_CEIL(glyph_metrics->height);
                break;
            }
        }
    }
}

const FT_Glyph_Metrics* font_ft::load_glyph(glyph_ft& in_glyph)
{
    in_glyph.glyph_index = FT_Get_Char_Index(face, in_glyph.codepoint);
    if(in_glyph.glyph_index == 0)
        return nullptr;
    FT_Error error = FT_Load_Glyph(face, in_glyph.glyph_index, load_flags);
    if(error)
    {
        //std::string err = get_ft_error_str(error);
        return nullptr;
    }

    // Need an outline for this to work
    FT_GlyphSlot slot = face->glyph;
    assert(slot->format == FT_GLYPH_FORMAT_OUTLINE);

    // Apply convenience transform (this is not picking from real "Bold"/"Italic" fonts! Merely applying
    // FreeType helper transform. Oblique == Slanting)
    if(user_flags & rasterizer_flags::bold)
        FT_GlyphSlot_Embolden(slot);
    if(user_flags & rasterizer_flags::oblique)
    {
        FT_GlyphSlot_Oblique(slot);
        // FT_BBox bbox;
        // FT_Outline_Get_BBox(&slot->outline, &bbox);
        // slot->metrics.width = bbox.xMax - bbox.xMin;
        // slot->metrics.height = bbox.yMax - bbox.yMin;
    }

    return &slot->metrics;
}

const FT_Bitmap* font_ft::render_glyph_and_get_info(glyph_info_ft* out_glyph_info)
{
    FT_GlyphSlot slot = face->glyph;
    FT_Error error = FT_Render_Glyph(slot, render_mode);
    if(error != 0)
        return nullptr;

    FT_Bitmap* ft_bitmap = &face->glyph->bitmap;
    out_glyph_info->width = uint32_t(ft_bitmap->width);
    out_glyph_info->height = uint32_t(ft_bitmap->rows);
    out_glyph_info->offset_x = face->glyph->bitmap_left;
    out_glyph_info->offset_y = -face->glyph->bitmap_top;
    out_glyph_info->advance_x = FT_CEIL(slot->advance.x);

    return ft_bitmap;
}

void font_ft::blit_glyph(const FT_Bitmap* ft_bitmap, uint8_t* dst, uint32_t dst_pitch,
                         const uint8_t* multiply_table)
{
    assert(ft_bitmap != nullptr);
    const auto w = uint32_t(ft_bitmap->width);
    const auto h = uint32_t(ft_bitmap->rows);
    const auto src_pitch = ft_bitmap->pitch;
    auto src = ft_bitmap->buffer;

    switch(ft_bitmap->pixel_mode)
    {
        case FT_PIXEL_MODE_GRAY: // Grayscale image, 1 byte per pixel.
        {
            if(multiply_table == nullptr)
            {
                for(uint32_t y = 0; y < h; y++, src += src_pitch, dst += dst_pitch)
                    memcpy(dst, src, w);
            }
            else
            {
                for(uint32_t y = 0; y < h; y++, src += src_pitch, dst += dst_pitch)
                    for(uint32_t x = 0; x < w; x++)
                        dst[x] = multiply_table[src[x]];
            }
            break;
        }
        case FT_PIXEL_MODE_MONO: // Monochrome image, 1 bit per pixel. The bits in each byte are ordered from
            // MSB to LSB.
        {
            uint8_t color0 = multiply_table ? multiply_table[0] : 0;
            uint8_t color1 = multiply_table ? multiply_table[255] : 255;
            for(uint32_t y = 0; y < h; y++, src += src_pitch, dst += dst_pitch)
            {
                uint8_t bits = 0;
                const uint8_t* bits_ptr = src;
                for(uint32_t x = 0; x < w; x++, bits <<= 1)
                {
                    if((x & 7) == 0)
                        bits = *bits_ptr++;
                    dst[x] = (bits & 0x80) ? color1 : color0;
                }
            }
            break;
        }
        default:
            assert(0 && "FreeTypeFont::BlitGlyph(): Unknown bitmap pixel mode!");
    }
}

void font_atlas_build_multiply_calc_lookup_table(uint8_t out_table[256], float in_brighten_factor)
{
    for(unsigned int i = 0; i < 256; i++)
    {
        auto value = static_cast<unsigned int>(i * in_brighten_factor);
        out_table[i] = value > 255 ? 255 : (value & 0xFF);
    }
}

struct font_info_build_src_data
{
    font_ft font{};
    // Rectangle to pack. We first fill in their size and the packer will give us their position.
    stbrp_rect* rects{};
    // Ranges as requested by user (user is allowed to request too much, e.g. 0x0020..0xFFFF)
    const font_wchar* src_ranges{};
    // Index into atlas->Fonts[] and dst_tmp_array[]
    int dst_index{};
    // Highest requested codepoint
    int glyphs_highest{};
    // Glyph count (excluding missing glyphs and glyphs already set by an earlier source font)
    int glyphs_count{};
    // Glyph bit map (random access, 1-bit per codepoint. This will be a maximum of 8KB)
    bit_vector glyphs_set{};
    std::vector<glyph_ft> glyphs_list{};
};

// Temporary data for one destination ImFont* (multiple source fonts can be merged into one destination
// ImFont)
struct font_info_build_dst_data
{
    // Number of source fonts targeting this destination font.
    int src_count{};
    int glyphs_highest{};
    int glyphs_count{};
    // This is used to resolve collision when multiple sources are merged into a same destination font.
    bit_vector glyphs_set{};
};

void unpack_bit_vector_to_flat_index_list(const bit_vector* in, std::vector<glyph_ft>* out)
{
    assert(sizeof(in->storage[0]) == sizeof(int));
    int idx = 0;
    for(auto entries_32 : in->storage)
    {
        for(int bit_n = 0; bit_n < 32; bit_n++)
        {
            if(uint32_t(entries_32) & (1u << bit_n))
            {
                glyph_ft glyph{};
                glyph.codepoint = font_wchar((idx << 5) + bit_n);
                out->push_back(glyph);
            }
        }
        idx++;
    }
}

bool build(FT_Library ft_library, font_atlas* atlas, std::string& err, unsigned int extra_flags)
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
        {
            err = "cfg.DstFont not pointing within atlas->fonts[] array?";
            return false;
        }

        font_ft& font_face = src_tmp.font;

        //-----------------------------
        // Load font
        if(!font_face.init(ft_library, cfg, extra_flags))
        {
            err = "Could not load font via Freetype";
            return false;
        }
        //-----------------------------

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
            for(font_wchar codepoint = src_range[0]; codepoint <= src_range[1]; codepoint++)
            {
                if(dst_tmp.glyphs_set.get_bit(int(codepoint))) // Don't overwrite existing glyphs. We could make
                    // this an option (e.g. MergeOverwrite)
                    continue;
                uint32_t glyph_index =
                    FT_Get_Char_Index(src_tmp.font.face, FT_ULong(codepoint)); // It is actually in the font?
                // (FIXME-OPT: We are not storing the
                // glyph_index..)
                if(glyph_index == 0)
                    continue;

                // Add to avail set/counters
                src_tmp.glyphs_count++;
                dst_tmp.glyphs_count++;
                src_tmp.glyphs_set.set_bit(int(codepoint), true);
                dst_tmp.glyphs_set.set_bit(int(codepoint), true);
                total_glyphs_count++;
            }
    }

    // 3. Unpack our bit map into a flat list (we now have all the Unicode points that we know are requested
    // _and_ available _and_ not overlapping another)
    for(auto& src_tmp : src_tmp_array)
    {        
        src_tmp.glyphs_list.reserve(size_t(src_tmp.glyphs_count));
        unpack_bit_vector_to_flat_index_list(&src_tmp.glyphs_set, &src_tmp.glyphs_list);
        src_tmp.glyphs_set.clear();
        assert(src_tmp.glyphs_list.size() == size_t(src_tmp.glyphs_count));
    }
    for(auto& dst_i : dst_tmp_array)
        dst_i.glyphs_set.clear();
    dst_tmp_array.clear();

    // Allocate packing character data and flag packed characters buffer as non-packed (x0=y0=x1=y1=0)
    // (We technically don't need to zero-clear buf_rects, but let's do it for the sake of sanity)
    std::vector<stbrp_rect> buf_rects;
    buf_rects.resize(total_glyphs_count);

    // Allocate temporary rasterization data buffers.
    // We could not find a way to retrieve accurate glyph size without rendering them.
    // (e.g. slot->metrics->width not always matching bitmap->width, especially considering the Oblique
    // transform) We allocate in chunks of 256 KB to not waste too much extra memory ahead. Hopefully users of
    // FreeType won't find the temporary allocations.
    const int chunk_size = 256 * 1024;
    uint32_t buf_bitmap_current_used_bytes = 0;
    std::vector<std::vector<uint8_t>> buf_bitmap_buffers;
    buf_bitmap_buffers.emplace_back(chunk_size);

    // 4. Gather glyphs sizes so we can pack them in our virtual canvas.
    // 8. Render/rasterize font characters into the texture
    int total_surface = 0;
    size_t buf_rects_out_n = 0;
    uint32_t max_rect_w = 0;

    for(size_t src_i = 0; src_i < src_tmp_array.size(); src_i++)
    {
        auto& src_tmp = src_tmp_array[src_i];
        auto& cfg = atlas->config_data[src_i];
        if(src_tmp.glyphs_count == 0)
            continue;

        src_tmp.rects = &buf_rects[buf_rects_out_n];
        buf_rects_out_n += size_t(src_tmp.glyphs_count);

        // Compute multiply table if requested
        const bool multiply_enabled = (cfg.rasterizer_multiply != 1.0f);
        uint8_t multiply_table[256];
        if(multiply_enabled)
            font_atlas_build_multiply_calc_lookup_table(multiply_table, cfg.rasterizer_multiply);

        // Gather the sizes of all rectangles we will need to pack
        const auto padding = stbrp_coord(atlas->tex_glyph_padding);
        for(size_t glyph_i = 0; glyph_i < src_tmp.glyphs_list.size(); glyph_i++)
        {
            auto& src_glyph = src_tmp.glyphs_list[glyph_i];

            const FT_Glyph_Metrics* metrics = src_tmp.font.load_glyph(src_glyph);
            //assert(metrics != nullptr);
            if(metrics == nullptr)
                continue;

            // Render glyph into a bitmap (currently held by FreeType)
            const FT_Bitmap* ft_bitmap = src_tmp.font.render_glyph_and_get_info(&src_glyph.info);
            assert(ft_bitmap);

            // Allocate new temporary chunk if needed
            const uint32_t bitmap_size_in_bytes = src_glyph.info.width * src_glyph.info.height;
            if(buf_bitmap_current_used_bytes + bitmap_size_in_bytes > chunk_size)
            {
                buf_bitmap_current_used_bytes = 0;
                buf_bitmap_buffers.emplace_back(chunk_size);
            }

            // Blit rasterized pixels to our temporary buffer and keep a pointer to it.
            src_glyph.bitmap_data = buf_bitmap_buffers.back().data() + buf_bitmap_current_used_bytes;
            buf_bitmap_current_used_bytes += bitmap_size_in_bytes;
            src_tmp.font.blit_glyph(ft_bitmap, src_glyph.bitmap_data, src_glyph.info.width * 1,
                                    multiply_enabled ? multiply_table : nullptr);

            src_tmp.rects[glyph_i].w = stbrp_coord(src_glyph.info.width + padding);
            src_tmp.rects[glyph_i].h = stbrp_coord(src_glyph.info.height + padding);
            max_rect_w = std::max(max_rect_w, uint32_t(src_tmp.rects[glyph_i].w) + padding);
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
    atlas->tex_width = estimate_width(tex_max, std::max(uint32_t(256), max_rect_w), surface_sqrt);

    // 5. Start packing
    // Pack our extra data rectangles first, so it will be on the upper-left corner of our texture (UV will
    // have small values).
    const auto num_nodes_for_packing_algorithm = atlas->tex_width - atlas->tex_glyph_padding;
    std::vector<stbrp_node> pack_nodes;
    pack_nodes.resize(num_nodes_for_packing_algorithm);
    stbrp_context pack_context;
    stbrp_init_target(&pack_context, int(atlas->tex_width - atlas->tex_glyph_padding),
                      int((1024 * 64) - atlas->tex_glyph_padding), pack_nodes.data(), int(pack_nodes.size()));

    // 6. Pack each source font. No rendering yet, we are working with rectangles in an infinitely tall
    // texture at this point.
    for(auto& src_tmp : src_tmp_array)
    {
        if(src_tmp.glyphs_count == 0)
            continue;

        stbrp_pack_rects(&pack_context, src_tmp.rects, src_tmp.glyphs_count);

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
        err = "No glyphs were loaded.";
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
    // 8. Copy rasterized font characters back into the main texture
    // 9. Setup ImFont and glyphs for runtime
    for(size_t src_i = 0; src_i < src_tmp_array.size(); src_i++)
    {
        auto& src_tmp = src_tmp_array[src_i];
        if(src_tmp.glyphs_count == 0)
            continue;

        auto& cfg = atlas->config_data[src_i];
        // We can have multiple input fonts writing into a same destination
        // font (when using MergeMode=true)
        auto& dst_font = cfg.dst_font;
        if(cfg.merge_mode)
        {
            if(!dst_font->build_lookup_table(err))
            {
                return false;
            }
        }

        const float ascent = src_tmp.font.info.ascender;
        const float descent = src_tmp.font.info.descender;
        const float line_gap = src_tmp.font.info.line_gap;
        const float ysuperscript_size = src_tmp.font.info.ysuperscript_size;
        const float ysuperscript_offset = src_tmp.font.info.ysuperscript_offset;
        const float ysubscript_size = src_tmp.font.info.ysubscript_size;
        const float ysubscript_offset = src_tmp.font.info.ysubscript_offset;
        const float xheight = src_tmp.font.info.xheight;
        const float cap_height = src_tmp.font.info.cap_height;

        const float line_height = (ascent - descent) + line_gap;

        atlas->setup_font(dst_font, &cfg, ascent, descent, line_height, xheight, cap_height,
                          ysuperscript_size, ysuperscript_offset, ysubscript_size, ysubscript_offset);

        const float font_off_x = cfg.glyph_offset_x;
        const float font_off_y = cfg.glyph_offset_y;
        bool has_kerning_table = FT_HAS_KERNING(src_tmp.font.face);

        const auto padding = int(atlas->tex_glyph_padding);
        for(int glyph_i = 0; glyph_i < src_tmp.glyphs_count; glyph_i++)
        {
            auto& src_glyph = src_tmp.glyphs_list[size_t(glyph_i)];

            if (cfg.merge_mode && dst_font->find_glyph_no_fallback(font_wchar(src_glyph.codepoint)))
                continue;

            auto& pack_rect = src_tmp.rects[size_t(glyph_i)];
            if(!pack_rect.was_packed)
            {
                err = "Could not pack all glyphs.";
                return false;
            }
            auto& info = src_glyph.info;


            //assert(info.width + padding <= pack_rect.w);
            //assert(info.height + padding <= pack_rect.h);
            const int tx = pack_rect.x + padding / 2;
            const int ty = pack_rect.y + padding / 2;

            // Blit from temporary buffer to final texture
            auto blit_src_stride = size_t(src_glyph.info.width);
            auto blit_dst_stride = size_t(atlas->tex_width);
            uint8_t* blit_src = src_glyph.bitmap_data;
            uint8_t* blit_dst = atlas->tex_pixels_alpha8.data() + (size_t(ty) * blit_dst_stride) + size_t(tx);
            for(auto y = int(info.height); y > 0; y--, blit_dst += blit_dst_stride, blit_src += blit_src_stride)
                std::memcpy(blit_dst, blit_src, blit_src_stride);

            float char_advance_x_org = info.advance_x;
            const float char_advance_x_mod =
                clamp(char_advance_x_org, cfg.glyph_min_advance_x, cfg.glyph_max_advance_x);
            float char_off_x = font_off_x;
            if(std::fabs(char_advance_x_org - char_advance_x_mod) > std::numeric_limits<float>::epsilon())
                char_off_x += cfg.pixel_snap_h
                                  ? float(int((char_advance_x_mod - char_advance_x_org) * 0.5f))
                                  : (char_advance_x_mod - char_advance_x_org) * 0.5f;
            // Register glyph
            auto x0 = float(info.offset_x);
            auto y0 = float(info.offset_y);
            float x1 = x0 + info.width;
            float y1 = y0 + info.height;
            float u0 = float(tx) / float(atlas->tex_width);
            float v0 = float(ty) / float(atlas->tex_height);

            float u1 = (float(tx) + info.width) / float(atlas->tex_width);
            float v1 = (float(ty) + info.height) / float(atlas->tex_height);

            // if no kerning table, don't waste time looking
            if(has_kerning_table && cfg.kerning_glyphs_limit > uint32_t(src_tmp.glyphs_count))
            {
                const auto codepoint = src_glyph.codepoint;
                const auto glyph_index = src_glyph.glyph_index;

                for(int glyph_j = 0; glyph_j < src_tmp.glyphs_count; glyph_j++)
                {
                    const auto codepoint_from = src_tmp.glyphs_list[size_t(glyph_j)].codepoint;
                    const auto glyph_index_from = src_tmp.glyphs_list[size_t(glyph_j)].glyph_index;

                    FT_Vector kerning{};
                    FT_Get_Kerning(src_tmp.font.face, glyph_index_from, glyph_index, FT_KERNING_DEFAULT, &kerning);
                    if(kerning.x != 0)
                    {
                        auto cp_from = font_wchar(codepoint_from);
                        auto cp_to = font_wchar(codepoint);

                        auto kern_value = FT_CEIL(kerning.x);
                        dst_font->kernings[{cp_from, cp_to}] = kern_value;
                    }
                }
            }

            dst_font->add_glyph(font_wchar(src_glyph.codepoint), x0 + char_off_x, y0 + font_off_y, x1 + char_off_x, y1 + font_off_y, u0, v0, u1, v1,
                                char_advance_x_mod);

        }

        src_tmp.rects = nullptr;
    }

    return atlas->finish(err);
}
}

bool build(font_atlas* atlas, std::string& err, unsigned int extra_flags)
{

    // https://www.freetype.org/freetype2/docs/reference/ft2-module_management.html#FT_New_Library
    FT_Library ft_library;
    FT_Error error = FT_Init_FreeType(&ft_library);
    if(error != 0)
    {
        err = "Could not init Freetype";
        return false;
    }
    // If you don't call FT_Add_Default_Modules() the rest of code may work, but FreeType won't use our custom
    // allocator.
    FT_Add_Default_Modules(ft_library);

    bool ret = build(ft_library, atlas, err, extra_flags);
    FT_Done_FreeType(ft_library);

    return ret;
}
}
}
