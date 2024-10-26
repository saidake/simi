package com.simi.sgz.AAAconfig;

import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

public class ThreadPoolContext {
    private volatile static ThreadPoolExecutor threadPoolExecutor;
    public static ThreadPoolExecutor getThreadPoolExecutor() {
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
        return threadPoolExecutor;
    }
}
