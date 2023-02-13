package com.saidake.trade.AAAconfig;

import com.saidake.common.core.config.http.ServletAttributesTaskDecorator;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.task.TaskExecutor;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

@Configuration
public class ThreadPoolConfig {
    @Bean("defaultAsyncExecutor")
    public TaskExecutor getDefaultAsyncExecutor(){
        ThreadPoolTaskExecutor executor=new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(10);
        executor.setMaxPoolSize(20);
        executor.setWaitForTasksToCompleteOnShutdown(true);
        executor.setThreadNamePrefix("Async-Default-");
        executor.setTaskDecorator(new ServletAttributesTaskDecorator());
        return executor;
    }
}
