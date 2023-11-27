package com.simi.test.thread;

import java.util.concurrent.*;

public class ThreadPoolTest {
    public static void main(String[] args) {
        ExecutorService executorService1 = Executors.newFixedThreadPool(30);
        for (int i = 0; i < 300; i++) {
            executorService1.submit(()->{
                try {
                    Thread.sleep(3000);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                System.out.println(Thread.currentThread().getName()+"test pool1");
            });
        }
        ExecutorService executorService2 = Executors.newFixedThreadPool(30);
        for (int i = 0; i < 300; i++) {
            executorService2.submit(()->{
                try {
                    Thread.sleep(3000);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                System.out.println(Thread.currentThread().getName()+"test pool2");
            });
        }
        ExecutorService executorService3= new ThreadPoolExecutor(
                10,
                30,
                1,
                TimeUnit.HOURS,
                new ArrayBlockingQueue(10));

        executorService1.shutdown();
        executorService2.shutdown();
        executorService3.shutdown();
    }
}
