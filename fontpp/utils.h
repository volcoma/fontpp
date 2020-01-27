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
#include <string>
#include <unordered_map>
#include <vector>

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
		if(surface_sqrt >= width * 0.67f)
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

/// \file parallel-util.hpp

#ifndef parallel_util_hpp
#define parallel_util_hpp

// Uncommenting the following line enables the "verbose" mode, which may be
// useful for debugging. However, notice that it affects the overall performance
// badly as it inserts mutex-locked standard output commands.

//#define PARALLELUTIL_VERBOSE

#include <functional>
#include <mutex>
#include <queue>
#include <thread>
#include <vector>
#ifdef PARALLELUTIL_VERBOSE
#include <iostream>
#endif
#include <condition_variable>
#include <future>
#include <stdexcept>

namespace parallel
{

class detached_thread_pool
{
public:
	detached_thread_pool(size_t threads = std::thread::hardware_concurrency());
    ~detached_thread_pool();

	template <class F, class... Args>
	auto enqueue(F&& f, Args&&... args) -> std::future<typename std::result_of<F(Args...)>::type>
	{
		using return_type = typename std::result_of<F(Args...)>::type;

		auto task = std::make_shared<std::packaged_task<return_type()>>(
			std::bind(std::forward<F>(f), std::forward<Args>(args)...));

		std::future<return_type> res = task->get_future();
		{
			std::lock_guard<std::mutex> lock(queue_mutex);

			// don't allow enqueueing after stopping the pool
			if(stop)
				throw std::runtime_error("enqueue on stopped ThreadPool");

			tasks.emplace([task]() { (*task)(); });
		}
		condition.notify_one();
		return res;
	}

private:
	std::vector<std::thread> workers;
	std::queue<std::function<void()>> tasks;

	std::mutex queue_mutex;
	std::condition_variable condition;
	bool stop{};
};

detached_thread_pool& get_pool();
/// \brief Execute a for-loop process for an array in parallel
/// \param n The number of iterations. I.e., { 0, 1, ..., n - 1 } will be visited.
/// \param function The function that will be called in the for-loop. This can be specified as a lambda
/// expression. The type should be equivalent to std::function<void(int)>. \param target_concurrency The
/// number of jobs that will be generated. When this is set to zero (which is the default), the hardware
/// concurrency will be automatically used.
template <typename Callable>
void parallel_for(int n, Callable function, int target_concurrency = 0)
{
	const int hint = (target_concurrency == 0) ? int(std::thread::hardware_concurrency()) : target_concurrency;
	const int jobs = std::min(n, (hint == 0) ? 4 : hint);
#ifdef PARALLELUTIL_VERBOSE
	std::mutex io_mutex;
#endif

	const int n_max_tasks_per_thread = (n / jobs) + (n % jobs == 0 ? 0 : 1);
	const int n_lacking_tasks = n_max_tasks_per_thread * jobs - n;

	auto inner_loop = [&](const int thread_index) {
		const int n_lacking_tasks_so_far = std::max(thread_index - jobs + n_lacking_tasks, 0);
		const int inclusive_start_index = thread_index * n_max_tasks_per_thread - n_lacking_tasks_so_far;
		const int exclusive_end_index = inclusive_start_index + n_max_tasks_per_thread -
										(thread_index - jobs + n_lacking_tasks >= 0 ? 1 : 0);

		for(int k = inclusive_start_index; k < exclusive_end_index; ++k)
		{
#ifdef PARALLELUTIL_VERBOSE
			io_mutex.lock();
			std::cout << "parallel-util ... Thread " << thread_index + 1 << ": "
					  << k - inclusive_start_index + 1 << " / " << exclusive_end_index - inclusive_start_index
					  << std::endl;
			io_mutex.unlock();
#endif
			function(k);
		}
#ifdef PARALLELUTIL_VERBOSE
		io_mutex.lock();
		std::cout << "parallel-util ... Thread " << thread_index + 1 << ": done" << std::endl;
		io_mutex.unlock();
#endif
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

/// \brief Execute a for-loop process for a 2D array (e.g., a bitmap image data) in parallel
/// \param width The width of the target 2D array. I.e., { 0, 1, ..., width - 1 } will be visited as the first
/// dimensional indices. \param height The height of the target 2D array. I.e., { 0, 1, ..., height - 1 } will
/// be visited as the second dimensional indices. \param function The function that will be called in the
/// for-loop. This can be specified as a lambda expression. The type should be equivalent to
/// std::function<void(int, int)>. \param target_concurrency The number of jobs that will be generated. When
/// this is set to zero (which is the default), the hardware concurrency will be automatically used.
template <typename Callable>
void parallel_for_2d(int width, int height, Callable function, int target_concurrency = 0)
{
	const int hint = (target_concurrency == 0) ? int(std::thread::hardware_concurrency()) : target_concurrency;
	const int jobs = std::min(width * height, (hint == 0) ? 4 : hint);

	auto inner_loop = [width, height, jobs, function = std::move(function)](const int thread_index)
	{
		const int n = width * height;

		const int start_index = thread_index * (n / jobs);
		const int end_index = (thread_index + 1 == jobs) ? n : (thread_index + 1) * (n / jobs);

		for(int k = start_index; k < end_index; ++k)
		{
			function(k % width, k / width);
		}
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
/// \brief Execute a for-loop process for an array in parallel based on a task queue
/// \param n The number of iterations. I.e., { 0, 1, ..., n - 1 } will be visited.
/// \param function The function that will be called in the for-loop. This can be specified as a lambda
/// expression. The type should be equivalent to std::function<void(int)>. \param target_concurrency The
/// number of threads that will be generated. When this is set to zero (which is the default), the hardware
/// concurrency will be automatically used. \details Compared to parallel_for(), this function is likely to
/// achieve better CPU occupancy especially when the cost of each local process is computationally
/// heterogenous (i.e., some processes are light and others are heavy). However, this function could be slower
/// than parallel_for() in some cases because of (1) cache inefficiency (each thread works on less local
/// processes) and (2) mutex lock for the task queue.
template <typename Callable>
void queue_based_parallel_for(int n, Callable function, int target_concurrency = 0)
{
	const int hint = (target_concurrency == 0) ? int(std::thread::hardware_concurrency()) : target_concurrency;
	const int jobs = std::min(n, (hint == 0) ? 4 : hint);

	// Mutex object for queue manipulation
	std::mutex queue_mutex;

#ifdef PARALLELUTIL_VERBOSE
	// Mutex object for IO
	std::mutex io_mutex;
#endif

	std::queue<int> task_queue;
	for(int i = 0; i < n; ++i)
	{
		task_queue.push(i);
	}

	auto thread_routine = [&](const int thread_index) {
		(void)thread_index;
		while(true)
		{
			queue_mutex.lock();

			if(task_queue.empty())
			{
				queue_mutex.unlock();
				break;
			}

			const int task_index = task_queue.front();
			task_queue.pop();

			queue_mutex.unlock();

#ifdef PARALLELUTIL_VERBOSE
			io_mutex.lock();
			std::cout << "parallel-util ... Thread " << thread_index + 1 << ": " << task_index + 1 << " / "
					  << n << std::endl;
			io_mutex.unlock();
#endif

			function(task_index);
		}

#ifdef PARALLELUTIL_VERBOSE
		io_mutex.lock();
		std::cout << "parallel-util ... Thread " << thread_index + 1 << ": done" << std::endl;
		io_mutex.unlock();
#endif
	};

	std::vector<std::future<void>> futures;
    futures.reserve(size_t(jobs));

	for(int j = 0; j < jobs; ++j)
	{
		futures.emplace_back(get_pool().enqueue(thread_routine, j));
//		futures.emplace_back(std::async(std::launch::async, thread_routine, j));
	}
	for(auto& t : futures)
	{
		t.wait();
	}
}

/// \brief Execute "map" operation to an array in parallel
/// \param input_array The array that will be processed by map operation.
/// \param function The function that will be used for map operation. It can be specified as a lambda
/// expression. It should take a single input variable whose type is T and returns a non-void class instance
/// etc. \return An array of resulting instances.
template <typename T, typename Callable>
auto parallel_map(const std::vector<T>& input_array, Callable function, int target_concurrency = 0)
#if __cplusplus < 201402L
	-> std::vector<decltype(function(input_array.front()))>
#endif
{
	std::vector<decltype(function(input_array.front()))> result_array(input_array.size());
	auto indexed_function = [&](int index) { result_array[index] = function(input_array[index]); };
	queue_based_parallel_for(input_array.size(), indexed_function, target_concurrency);
	return result_array;
}

/// \brief Execute a set of processes in parallel
inline void parallel_exec(const std::vector<std::function<void()>>& functions)
{
	queue_based_parallel_for(int(functions.size()), [&](int index) { functions[size_t(index)](); });
}
}

#endif /* parallel_util_hpp */
