#include "fontpp/font.h"
#include <iostream>
int main()
{
    fnt::font_glyph_ranges_builder builder{};
	builder.add_ranges(fnt::get_glyph_ranges_all());

    fnt::font_atlas atlas{};
    atlas.add_font_default();

    std::string err{};
	if(!atlas.build(err))
	{
		std::cout << "Failed to build atlas : " << err << std::endl;
		return -1;
	}

    uint32_t w{};
    uint32_t h{};
    uint32_t bpp{};
    uint8_t* pixels{};
    //atlas.get_tex_data_as_rgba32(&pixels, &w, &h, &bpp);
	atlas.get_tex_data_as_rgba8(&pixels, &w, &h, &bpp);
	
	return 0;
}
