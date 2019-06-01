#include "fontpp/font.h"
#include "fontpp/stb/stb.h"
int main()
{
    fnt::font_glyph_ranges_builder builder{};
    builder.add_ranges(fnt::get_glyph_ranges_default());

    fnt::font_atlas atlas{};
    atlas.add_font_default();

    std::string err{};
    atlas.build(fnt::font_rasterizer::stb, err);

    uint32_t w{};
    uint32_t h{};
    uint32_t bpp{};
    uint8_t* pixels{};
    atlas.get_tex_data_as_rgba32(&pixels, &w, &h, &bpp);

	return 0;
}
