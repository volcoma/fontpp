/*
 Copyright (C) 2014 Mikko Mononen (memon@inside.org)
 Copyright (C) 2009-2012 Stefan Gustavson (stefan.gustavson@gmail.com)

 Permission is hereby granted, free of charge, to any person obtaining a copy
 of this software and associated documentation files (the "Software"), to deal
 in the Software without restriction, including without limitation the rights
 to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in
 all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 THE SOFTWARE.
 */

#ifndef SDF_H
#define SDF_H

#include <cstdint>

// assumes input and output are big enough to NOT check bounds
// input can be anti-aliased
void brute_force_sdf(uint8_t* output, const uint8_t* input, int width, int height, int spread);
void brute_force_sdf_parallel(uint8_t* output, const uint8_t* input, int width, int height, int spread);


// Sweep-and-update Euclidean distance transform of an antialised image for contour textures.
// Based on edtaa3func.c by Stefan Gustavson.
//
// White (255) pixels are treated as object pixels, zero pixels are treated as background.
// An attempt is made to treat antialiased edges correctly. The input image must have
// pixels in the range [0,255], and the antialiased image should be a box-filter
// sampling of the ideal, crisp edge. If the antialias region is more than 1 pixel wide,
// the result from this transform will be inaccurate.
// Pixels at image border are not calculated and are set to 0.
//
// The output distance field is encoded as bytes, where 0 = radius (outside) and 255 = -radius (inside).
// Input and output can be the same buffer.
//   out - Output of the distance transform, one byte per pixel.
//   outstride - Bytes per row on output image.
//   radius - The radius of the distance field narrow band in pixels.
//   img - Input image, one byte per pixel.
//   width - Width of the image.
//   height - Height of the image.
//   stride - Bytes per row on input image.
//   max_allowed_memory_usage_hint - Desired memory usage in bytes only @parallel=true. Minimal is 3KB * sizeof(float).
void sweep_and_update_sdf(unsigned char* out, int outstride, float radius,
             const unsigned char* img, int width, int height, int stride,
             bool parallel = true,
             int max_allowed_memory_usage_hint = 1024 * 1024 * 2 * (sizeof(float) * 3));

#endif //SDF_H
