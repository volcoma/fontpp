#include "utils.h"

namespace parallel
{

thread_pool::thread_pool(size_t threads)
{
    condition = std::make_shared<std::condition_variable>();
    workers.reserve(threads);
    for(size_t i = 0; i < threads; ++i)
    {
        workers.emplace_back([this, condition = this->condition] {
            for(;;)
            {
                task_t task;

                {
                    std::unique_lock<std::mutex> lock(this->queue_mutex);
                    this->condition->wait(lock, [this]
                    {
                        return this->stop || !this->tasks.empty();
                    });
                    if(this->stop)
                    {
                        return;
                    }
                    task = std::move(this->tasks.front());
                    this->tasks.pop();
                }

                if(task)
                {
                    task();
                }
            }
        });
    }
}


thread_pool::~thread_pool()
{
    {
        std::unique_lock<std::mutex> lock(queue_mutex);
        stop = true;
    }

    condition->notify_all();

    for(auto& worker : workers)
    {
        if(worker.joinable())
        {
            worker.join();
        }
    }
}

thread_pool& get_pool()
{
    static thread_pool pool;
	return pool;
}

}
