#pragma once
#include <string>

namespace fnt
{
struct font_atlas;

namespace freetype
{
// Hinting greatly impacts visuals (and glyph sizes).
// When disabled, FreeType generates blurrier glyphs, more or less matches the stb's output.
// The Default hinting mode usually looks good, but may distort glyphs in an unusual way.
// The Light hinting mode generates fuzzier glyphs but better matches Microsoft's rasterizer.

// You can set those flags on a per font basis in ImFontConfig::RasterizerFlags.
// Use the 'extra_flags' parameter of BuildFontAtlas() to force a flag on all your fonts.
enum rasterizer_flags
{
    // By default, hinting is enabled and the font's native hinter is preferred over the auto-hinter.
    no_hinting = 1 << 0,      // Disable hinting. This generally generates 'blurrier' bitmap glyphs when the glyph
                              // are rendered in any of the anti-aliased modes.

    no_auto_hint = 1 << 1,    // Disable auto-hinter.
    force_auto_hint = 1 << 2, // Indicates that the auto-hinter is preferred over the font's native hinter.

    light_hinting = 1 << 3,   // A lighter hinting algorithm for gray-level modes. Many generated glyphs are
                              // fuzzier but better resemble their original shape. This is achieved by snapping
                              // glyphs to the pixel grid only vertically (Y-axis), as is done by Microsoft's
                              // ClearType and Adobe's proprietary font renderer. This preserves inter-glyph
                              // spacing in horizontal text.

    mono_hinting = 1 << 4,    // Strong hinting algorithm that should only be used for monochrome output.
    bold = 1 << 5,            // Styling: Should we artificially embolden the font?
    oblique = 1 << 6,         // Styling: Should we slant the font, emulating italic style?
    monochrome = 1 << 7       // Disable anti-aliasing. Combine this with MonoHinting for best results!
};

bool build(font_atlas* atlas, std::string& err, unsigned int extra_flags = 0);
}
}
