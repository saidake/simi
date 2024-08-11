package com.simi.AAAconfig;

import com.simi.AAAconfig.bean.SingletonBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import java.util.concurrent.Executor;
import java.util.concurrent.ThreadPoolExecutor;

@Configuration
public class GlobalConfiguration {

    @Bean(name = "customExecutor")
    public Executor taskExecutor() {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(2); // Minimum number of threads
        executor.setMaxPoolSize(3); // Maximum number of threads
        executor.setQueueCapacity(2); // Capacity of the queue
        executor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());
        executor.setThreadNamePrefix("Async-Craig-");
        executor.initialize();
        return executor;
    }

    @Bean
    public SingletonBean singletonBean() {
        return new SingletonBean();
    }

}
