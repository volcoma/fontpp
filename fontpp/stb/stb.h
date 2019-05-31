#pragma once
#include <string>

namespace fnt
{
struct font_atlas;

namespace stb
{

bool build(font_atlas* atlas, std::string& err);
}
} // namespace
