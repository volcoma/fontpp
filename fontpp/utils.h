#pragma once
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <limits>
#include <memory>
#include <vector>
#include <string>
#include <unordered_map>

namespace fnt
{

inline uint32_t upper_power_of_two(uint32_t v)
{
	v--;
	v |= v >> 1;
	v |= v >> 2;
	v |= v >> 4;
	v |= v >> 8;
	v |= v >> 16;
	v++;
	return v;
}

inline uint32_t estimate_width(uint32_t max, uint32_t min, uint32_t surface_sqrt)
{
	if(max < min)
	{
		return max;
	}

	uint32_t width = max;
	while(width > min)
	{
		if(surface_sqrt >= width * 0.7f)
		{
			return width;
		}

		width /= 2;
	}
	return width;
}

template <typename T>
inline T clamp(T v, T mn, T mx)
{
	return (v < mn) ? mn : (v > mx) ? mx : v;
}
// Helper: bool_vector. Store 1-bit per value.
// Note that Resize() currently clears the whole vector.
struct bool_vector
{
	std::vector<int> storage{};
	void resize(int sz)
	{
		clear();
		storage.resize(size_t((sz + 31) >> 5), 0);
	}
	void clear()
	{
		storage.clear();
	}
	bool get_bit(int n) const
	{
		int off = (n >> 5);
		int mask = 1 << (n & 31);
		return (storage[size_t(off)] & mask) != 0;
	}
	void set_bit(int n, bool v)
	{
		int off = (n >> 5);
		int mask = 1 << (n & 31);
		if(v)
			storage[size_t(off)] |= mask;
		else
			storage[size_t(off)] &= ~mask;
	}
};

} // namespace
