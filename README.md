[![Build Status](https://travis-ci.org/volcoma/fontpp.svg?branch=master)](https://travis-ci.org/volcoma/fontpp)
[![Build status](https://ci.appveyor.com/api/projects/status/kustjvv90nja8dxg?svg=true)](https://ci.appveyor.com/project/volcoma/fontpp)


## fontpp c++14 font loading library
- Supports loading of .ttf/.otf formats
- Supports 2 backend rasterizers - freetype/stb
- Supports fast SDF multithreaded generation, even suitable for runtime generation


```c++
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
    atlas.get_tex_data_as_alpha8(&pixels, &w, &h, &bpp);
    
    return 0;
}
```
