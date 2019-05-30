#pragma once
#include <cassert>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <limits>
#include <vector>

namespace fnt
{

// 2D vector (often used to store positions, sizes, etc.)
struct vec2
{
	float x, y;
	vec2()
	{
		x = y = 0.0f;
	}
	vec2(float _x, float _y)
	{
		x = _x;
		y = _y;
	}
};

static inline uint32_t upper_power_of_two(uint32_t v)
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

template <typename T>
static inline T clamp(T v, T mn, T mx)
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
