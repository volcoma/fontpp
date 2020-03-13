#include "sdf.h"
#include "utils.h"

#include <cmath>
#include <cstdlib>
#include <cfloat>
#define SDF_MAX_PASSES 1		// Maximum number of distance transform passes
#define SDF_SLACK 0.001f		// Controls how much smaller the neighbour value must be to cosnider, too small slack increse iteration count.
#define SDF_SQRT2 1.4142136f	// sqrt(2)
#define SDF_BIG 1e+37f			// Big value used to initialize the distance field.


/// Brute force

namespace
{

template <typename T>
inline T sq(T t)
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

    int startx = std::max<int>(0, x - spread);
    int endx = std::min<int>(width - 1, x + spread);
    int starty = std::max<int>(0, y - spread);
    int endy = std::min<int>(height - 1, y + spread);

    for(int cy = starty, offsety = starty - y; cy < endy; ++cy, ++offsety)
    {
        int offset = cy * width;
        int offsety_sq = sq(offsety);

        for(int cx = startx, offsetx = startx - x; cx < endx; ++cx, ++offsetx)
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

}

void brute_force_sdf(uint8_t* output, const uint8_t* input, int width, int height, int spread)
{
    for(int y = 0; y < height; ++y)
    {
        for(int x = 0; x < width; ++x)
        {
            int i = y * width + x;
            auto value = sample(input, width, height, x, y, spread);
            output[i] = uint8_t(std::round(fnt::clamp(value, 0.f, 1.f) * 255));
        }
    }
}

void brute_force_sdf_parallel(uint8_t* output, const uint8_t* input, int width, int height, int spread)
{
    parallel::parallel_for_2d(width, height, [&](int x, int y)
    {
        int i = y * width + x;

        auto value = sample(input, width, height, x, y, spread);
        output[i] = uint8_t(std::round(fnt::clamp(value, 0.f, 1.f) * 255));
    });
}

/// Sweep and update

namespace
{

float sdf__edgedf(float gx, float gy, float a)
{
    double df, glength, temp, a1;

    if ((gx == 0) || (gy == 0)) { // Either A) gu or gv are zero, or B) both
        df = 0.5-a;  // Linear approximation is A) correct or B) a fair guess
    } else {
        glength = sqrt(gx*gx + gy*gy);
        if(glength>0) {
            gx = gx/glength;
            gy = gy/glength;
        }
        /* Everything is symmetric wrt sign and transposition,
         * so move to first octant (gx>=0, gy>=0, gx>=gy) to
         * avoid handling all possible edge directions.
         */
        gx = fabs(gx);
        gy = fabs(gy);
        if(gx<gy) {
            temp = gx;
            gx = gy;
            gy = temp;
        }
        a1 = 0.5*gy/gx;
        if (a < a1) { // 0 <= a < a1
            df = 0.5*(gx + gy) - sqrt(2.0*gx*gy*a);
        } else if (a < (1.0-a1)) { // a1 <= a <= 1-a1
            df = (0.5-a)*gx;
        } else { // 1-a1 < a <= 1
            df = -0.5*(gx + gy) + sqrt(2.0*gx*gy*(1.0-a));
        }
    }
    return df;
}

struct SDFpoint {
    float x,y;
};

float sdf__distsqr(struct SDFpoint* a, struct SDFpoint* b)
{
    float dx = b->x - a->x, dy = b->y - a->y;
    return dx*dx + dy*dy;
}

float sdf__clamp01(float x)
{
    return x < 0.0f ? 0.0f : (x > 1.0f ? 1.0f : x);
}

struct rect
{
    int x;
    int y;
    int w;
    int h;
};

void sweep_and_update_block(const rect& block, unsigned char* out, int outstride, float radius,
                            const unsigned char* img, int width, int height, int stride)
{
    const bool has_left_shared_region = block.x > 0;
    const bool has_top_shared_region = block.y + block.h < height;
    const bool has_right_shared_region = block.x + block.w < width;
    const bool has_bottom_shared_region = block.y > 0;

    const auto shared_region = static_cast<int>(radius);
    rect whole_block; // the block including the shared regions
    whole_block.w = block.w + (has_left_shared_region ? shared_region : 0) + (has_right_shared_region ? shared_region : 0);
    whole_block.h = block.h + (has_top_shared_region ? shared_region : 0) + (has_bottom_shared_region ? shared_region : 0);
    whole_block.x = block.x - (has_left_shared_region ? shared_region : 0);
    whole_block.y = block.y - (has_bottom_shared_region ? shared_region : 0);

    const auto block_total_cells = static_cast<size_t>(whole_block.w) * static_cast<size_t>(whole_block.h);

    auto* temp = reinterpret_cast<uint8_t*>(malloc(size_t(block_total_cells) * sizeof(float) * 3)); // Huge block, might be no such continous free block in the memory...
    if(!temp)
    {
        return;
    }

    auto tdist = reinterpret_cast<float*>(&temp[0]);
    auto tpt = reinterpret_cast<struct SDFpoint*>(&temp[block_total_cells * sizeof(float)]);

    // Initialize buffers
    for (size_t i = 0; i < block_total_cells; i++)
    {
        tpt[i].x = 0.f;
        tpt[i].y = 0.f;
        tdist[i] = SDF_BIG;
    }

    // Calculate position of the anti-aliased pixels and distance to the boundary of the shape.
    for (auto y = 1; y < whole_block.h - 1; y++)
    {
        for (auto x = 1; x < whole_block.w - 1; x++)
        {
            auto original_x = x + whole_block.x;
            auto original_y = y + whole_block.y;

            int tk, k = original_x + original_y * stride;
            struct SDFpoint c = { (float)x, (float)y }; // TODO: is this correct?
            float d, gx, gy, glen;

            // Skip flat areas.
            if (img[k] == 255) continue;
            if (img[k] == 0) {
                // Special handling for cases where full opaque pixels are next to full transparent pixels.
                // See: https://github.com/memononen/SDF/issues/2
                int he = img[k-1] == 255 || img[k+1] == 255;
                int ve = img[k-stride] == 255 || img[k+stride] == 255;
                if (!he && !ve) continue;
            }

            // Calculate gradient direction
            gx = -(float)img[k-stride-1] - SDF_SQRT2*(float)img[k-1] - (float)img[k+stride-1] + (float)img[k-stride+1] + SDF_SQRT2*(float)img[k+1] + (float)img[k+stride+1];
            gy = -(float)img[k-stride-1] - SDF_SQRT2*(float)img[k-stride] - (float)img[k-stride+1] + (float)img[k+stride-1] + SDF_SQRT2*(float)img[k+stride] + (float)img[k+stride+1];
            if (fabsf(gx) < 0.001f && fabsf(gy) < 0.001f) continue;
            glen = gx*gx + gy*gy;
            if (glen > 0.0001f) {
                glen = 1.0f / sqrtf(glen);
                gx *= glen;
                gy *= glen;
            }

            // Find nearest point on contour.
            tk = x + y * whole_block.w;
            d = sdf__edgedf(gx, gy, (float)img[k]/255.0f);
            tpt[tk].x = x + gx*d;
            tpt[tk].y = y + gy*d;
            tdist[tk] = sdf__distsqr(&c, &tpt[tk]);
        }
    }

    // Calculate distance transform using sweep-and-update.
    for (auto pass = 0; pass < SDF_MAX_PASSES; pass++){
        int changed = 0;

        // Bottom-left to top-right.
        for (auto y = 1; y < whole_block.h - 1; y++)
        {
            for (auto x = 1; x < whole_block.w - 1; x++)
            {
                int k = x + y * whole_block.w, kn, ch = 0;
                struct SDFpoint c = { (float)x, (float)y }, pt;
                float pd = tdist[k], d;
                // (-1,-1)
                kn = k - 1 - whole_block.w;
                if (tdist[kn] < pd) {
                    d = sdf__distsqr(&c, &tpt[kn]);
                    if (d + SDF_SLACK < pd) {
                        pt = tpt[kn];
                        pd = d;
                        ch = 1;
                    }
                }
                // (0,-1)
                kn = k - whole_block.w;
                if (tdist[kn] < pd) {
                    d = sdf__distsqr(&c, &tpt[kn]);
                    if (d + SDF_SLACK < pd) {
                        pt = tpt[kn];
                        pd = d;
                        ch = 1;
                    }
                }
                // (1,-1)
                kn = k + 1 - whole_block.w;
                if (tdist[kn] < pd) {
                    d = sdf__distsqr(&c, &tpt[kn]);
                    if (d + SDF_SLACK < pd) {
                        pt = tpt[kn];
                        pd = d;
                        ch = 1;
                    }
                }
                // (-1,0)
                kn = k - 1;
                if (tdist[kn] < pd) {
                    d = sdf__distsqr(&c, &tpt[kn]);
                    if (d + SDF_SLACK < pd) {
                        pt = tpt[kn];
                        pd = d;
                        ch = 1;
                    }
                }
                if (ch) {
                    tpt[k] = pt;
                    tdist[k] = pd;
                    changed++;
                }
            }
        }

        // Top-right to bottom-left.
        for (auto y = whole_block.h - 2; y > 0 ; --y)
        {
            for (auto x = whole_block.w - 2; x > 0; --x)
            {
                int k = x+y*whole_block.w, kn, ch = 0;
                struct SDFpoint c = { (float)x, (float)y }, pt;
                float pd = tdist[k], d;
                // (1,0)
                kn = k + 1;
                if (tdist[kn] < pd) {
                    d = sdf__distsqr(&c, &tpt[kn]);
                    if (d + SDF_SLACK < pd) {
                        pt = tpt[kn];
                        pd = d;
                        ch = 1;
                    }
                }
                // (-1,1)
                kn = k - 1 + whole_block.w;
                if (tdist[kn] < pd) {
                    d = sdf__distsqr(&c, &tpt[kn]);
                    if (d + SDF_SLACK < pd) {
                        pt = tpt[kn];
                        pd = d;
                        ch = 1;
                    }
                }
                // (0,1)
                kn = k + whole_block.w;
                if (tdist[kn] < pd) {
                    d = sdf__distsqr(&c, &tpt[kn]);
                    if (d + SDF_SLACK < pd) {
                        pt = tpt[kn];
                        pd = d;
                        ch = 1;
                    }
                }
                // (1,1)
                kn = k + 1 + whole_block.w;
                if (tdist[kn] < pd) {
                    d = sdf__distsqr(&c, &tpt[kn]);
                    if (d + SDF_SLACK < pd) {
                        pt = tpt[kn];
                        pd = d;
                        ch = 1;
                    }
                }
                if (ch) {
                    tpt[k] = pt;
                    tdist[k] = pd;
                    changed++;
                }
            }
        }

        if (changed == 0) break;
    }

    // Map to good range.
    const float scale = 1.0f / radius;

    // Write the calculated distances
    for (auto y = block.y, endy = block.y + block.h; y < endy; ++y)
    {
        for (auto x = block.x, endx = block.x + block.w; x < endx; ++x)
        {
            const auto whole_block_x = x - block.x + (has_left_shared_region ? shared_region : 0);
            const auto whole_block_y = y - block.y + (has_bottom_shared_region ? shared_region : 0);
            float d = sqrtf(tdist[whole_block_x + whole_block_y * whole_block.w]) * scale;
            if (img[x + y * stride] > 127) d = -d;
            out[x + y * outstride] = (unsigned char)(sdf__clamp01(0.5f - d*0.5f) * 255.0f);
        }
    }

    // DEBUG: override white border on bottom and left edge to visualize each block borders
//    for (auto x = block.x, y = block.y, endy = block.y + block.h; y < endy; ++y)
//    {
//        out[x + y * outstride] = 255;
//    }
//    for (auto k = block.y * outstride + block.x, endk = k + block.w; k < endk; ++k)
//    {
//        out[k] = 255;
//    }

    free(temp);
}
}

void sweep_and_update_sdf(unsigned char* out, int outstride, float radius,
                          const unsigned char* img, int width, int height, int stride,
                          bool parallel,
                          int max_allowed_memory_usage_hint)
{
    if(!parallel)
    {
        sweep_and_update_block({0, 0, width, height}, out, outstride, radius, img, width, height, stride);
        return;
    }

    using namespace parallel;

    static constexpr const auto MEMORY_PER_PIXEL = static_cast<int>(sizeof(SDFpoint) + sizeof(float));
    max_allowed_memory_usage_hint = std::max(max_allowed_memory_usage_hint, 1024 * MEMORY_PER_PIXEL);

    const auto hint = std::min(4, static_cast<int>(std::thread::hardware_concurrency()));
    const auto max_pixels_per_block_memory_wise = (max_allowed_memory_usage_hint / hint) / MEMORY_PER_PIXEL;
    const auto max_pixels_per_block_thread_wise = (width * height) / hint;
    const auto max_pixels_per_block = std::min(max_pixels_per_block_memory_wise, max_pixels_per_block_thread_wise);
    auto block_side = static_cast<int>(std::floor(std::sqrt(max_pixels_per_block))); // using square blocks for simplicity

    auto rows = height / block_side + (height % block_side != 0 ? 1 : 0);
    auto cols = width / block_side + (width % block_side != 0 ? 1 : 0);

    int jobs = std::max(hint, rows * cols);

    const auto last_block_col_width = width - block_side * (cols - 1);
    const auto last_block_row_height = height - block_side * (rows - 1);

    auto inner_loop = [=](const int block_row_idx, const int block_col_idx)
    {
        rect block;
        block.x = block_col_idx * block_side;
        block.y = block_row_idx * block_side;
        block.w = (block_col_idx == cols - 1) ? last_block_col_width : block_side;
        block.h = (block_row_idx == rows - 1) ? last_block_row_height : block_side;

        sweep_and_update_block(block, out, outstride, radius, img, width, height, stride);
    };

    std::vector<std::future<void>> futures;
    futures.reserve(size_t(jobs));
    for(int r = 0; r < rows; ++r)
    {
        for(int c = 0; c < cols; ++c)
        {
            futures.emplace_back(get_pool().enqueue(inner_loop, r, c));
    //		futures.emplace_back(std::async(std::launch::async, inner_loop, r, c));
        }
    }
    for(auto& t : futures)
    {
        t.wait();
    }
}
