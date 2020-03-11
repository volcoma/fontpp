#include "edtaa3func.h"
#include "utils.h"

#include <cmath>
#include <cstdlib>
#include <cfloat>
#define SDF_MAX_PASSES 1		// Maximum number of distance transform passes
#define SDF_SLACK 0.001f		// Controls how much smaller the neighbour value must be to cosnider, too small slack increse iteration count.
#define SDF_SQRT2 1.4142136f	// sqrt(2)
#define SDF_BIG 1e+37f			// Big value used to initialize the distance field.

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
}

void sdf_build(unsigned char* out, int outstride, float radius,
					 const unsigned char* img, int width, int height, int stride)
{
    unsigned char* temp = (unsigned char*)malloc(width*height*sizeof(float)*3);

    int i, x, y, pass;
    float* tdist = (float*)&temp[0];
    struct SDFpoint* tpt = (struct SDFpoint*)&temp[width * height * sizeof(float)];

    // Initialize buffers
    for (i = 0; i < width*height; i++) {
        tpt[i].x = 0;
        tpt[i].y = 0;
        tdist[i] = SDF_BIG;
    }

    // Calculate position of the anti-aliased pixels and distance to the boundary of the shape.
    for (y = 1; y < height-1; y++) {
        for (x = 1; x < width-1; x++) {
            int tk, k = x + y * stride;
            struct SDFpoint c = { (float)x, (float)y };
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
            tk = x + y * width;
            d = sdf__edgedf(gx, gy, (float)img[k]/255.0f);
            tpt[tk].x = x + gx*d;
            tpt[tk].y = y + gy*d;
            tdist[tk] = sdf__distsqr(&c, &tpt[tk]);
        }
    }

    // Calculate distance transform using sweep-and-update.
    for (pass = 0; pass < SDF_MAX_PASSES; pass++){
        int changed = 0;

        // Bottom-left to top-right.
        for (y = 1; y < height-1; y++) {
            for (x = 1; x < width-1; x++) {
                int k = x+y*width, kn, ch = 0;
                struct SDFpoint c = { (float)x, (float)y }, pt;
                float pd = tdist[k], d;
                // (-1,-1)
                kn = k - 1 - width;
                if (tdist[kn] < pd) {
                    d = sdf__distsqr(&c, &tpt[kn]);
                    if (d + SDF_SLACK < pd) {
                        pt = tpt[kn];
                        pd = d;
                        ch = 1;
                    }
                }
                // (0,-1)
                kn = k - width;
                if (tdist[kn] < pd) {
                    d = sdf__distsqr(&c, &tpt[kn]);
                    if (d + SDF_SLACK < pd) {
                        pt = tpt[kn];
                        pd = d;
                        ch = 1;
                    }
                }
                // (1,-1)
                kn = k + 1 - width;
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
        for (y = height-2; y > 0 ; y--) {
            for (x = width-2; x > 0; x--) {
                int k = x+y*width, kn, ch = 0;
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
                kn = k - 1 + width;
                if (tdist[kn] < pd) {
                    d = sdf__distsqr(&c, &tpt[kn]);
                    if (d + SDF_SLACK < pd) {
                        pt = tpt[kn];
                        pd = d;
                        ch = 1;
                    }
                }
                // (0,1)
                kn = k + width;
                if (tdist[kn] < pd) {
                    d = sdf__distsqr(&c, &tpt[kn]);
                    if (d + SDF_SLACK < pd) {
                        pt = tpt[kn];
                        pd = d;
                        ch = 1;
                    }
                }
                // (1,1)
                kn = k + 1 + width;
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
    for (y = 0; y < height; y++) {
        for (x = 0; x < width; x++) {
            float d = sqrtf(tdist[x+y*width]) * scale;
            if (img[x+y*stride] > 127) d = -d;
            out[x+y*outstride] = (unsigned char)(sdf__clamp01(0.5f - d*0.5f) * 255.0f);
        }
    }

    free(temp);
}

struct point
{
    int x;
    int y;
};

struct rect
{
    int x;
    int y;
    int w;
    int h;
};

void sdf_build_parallel_block(const rect& block, unsigned char* out, int outstride, float radius,
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

    // DEBUG. override white border on bottom left edge
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

void sdf_build_parallel(unsigned char* out, int outstride, float radius,
                        const unsigned char* img, int width, int height, int stride)
{
    using namespace parallel;

    const auto hint = static_cast<int>(std::thread::hardware_concurrency());
    const int jobs = std::min(width * height, (hint == 0) ? 4 : hint); // why hardware concurrency 0 gives 4 jobs???

    // TODO: devide the rectangle into a grid of rects(multiple columns and rows, instead of just one column as it is now).
    // It will reduce the "shared" regions between the blocks, which are calculated/allocated per neighbour block.
    // The shared region is with size @radius over the shared length.
    int block_height = height / jobs;
    int last_block_height = height % jobs == 0 ? block_height : height % jobs;

    auto inner_loop = [=](const int thread_index)
    {
        rect block;
        block.x = 0;
        block.y = thread_index * block_height;
        block.w = width;
        block.h = (thread_index == jobs - 1) ? last_block_height : block_height;

        sdf_build_parallel_block(block, out, outstride, radius, img, width, height, stride);
    };

    std::vector<std::future<void>> futures;
    futures.reserve(size_t(jobs));
    for(int j = 0; j < jobs; ++j)
    {
        futures.emplace_back(get_pool().enqueue(inner_loop, j));
//		futures.emplace_back(std::async(std::launch::async, inner_loop, j));
    }
    for(auto& t : futures)
    {
        t.wait();
    }
}
