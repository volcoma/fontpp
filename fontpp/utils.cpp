#include "utils.h"

namespace parallel
{

detached_thread_pool::detached_thread_pool(size_t threads)
{
    workers.reserve(threads);
    for(size_t i = 0; i < threads; ++i)
    {
        workers.emplace_back([this] {
            for(;;)
            {
                std::function<void()> task;

                {
                    std::unique_lock<std::mutex> lock(this->queue_mutex);
                    this->condition.wait(lock, [this]
                    {
                        return this->stop || !this->tasks.empty();
                    });
                    if(this->stop)
                        return;
                    task = std::move(this->tasks.front());
                    this->tasks.pop();
                }

                task();
            }
        });

        workers.back().detach();
    }
}


detached_thread_pool::~detached_thread_pool() = default;

detached_thread_pool& get_pool()
{
    static detached_thread_pool pool;
	return pool;
}

}
