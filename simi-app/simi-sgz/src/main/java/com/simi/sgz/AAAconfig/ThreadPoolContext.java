package com.simi.sgz.AAAconfig;

import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

public class ThreadPoolContext {
    private volatile static ThreadPoolExecutor threadPoolExecutor;
    public static void executeTask(Runnable task) {
        if (threadPoolExecutor == null) {
            synchronized (ThreadPoolContext.class) {
                if (threadPoolExecutor == null) {
                    threadPoolExecutor =
                            new ThreadPoolExecutor(
                                    9, 10, 0L,
                                    TimeUnit.MILLISECONDS,
                                    new LinkedBlockingQueue<>(10));
                }
            }
        }
        threadPoolExecutor.execute(task);
    }

    public static void shutdown(){
        if(threadPoolExecutor!=null)threadPoolExecutor.shutdown();
    }
}
