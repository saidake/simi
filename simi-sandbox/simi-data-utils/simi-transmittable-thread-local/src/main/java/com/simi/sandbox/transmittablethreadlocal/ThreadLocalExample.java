package com.simi.sandbox.transmittablethreadlocal;
import com.alibaba.ttl.TransmittableThreadLocal;
import com.alibaba.ttl.TtlRunnable;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class ThreadLocalExample {

    private static ThreadLocal<String> inheritableThreadLocal = new InheritableThreadLocal<>();
    private static ThreadLocal<String> transmittableThreadLocal = new TransmittableThreadLocal<>();

    public static void main(String[] args) {
        ExecutorService executorService = Executors.newFixedThreadPool(1);

        // Set initial values
        inheritableThreadLocal.set("Initial Inheritable");
        transmittableThreadLocal.set("Initial Transmittable");

        System.out.println("Main Thread:");
        System.out.println("[Main] inheritableThreadLocal: " + inheritableThreadLocal.get());
        System.out.println("[Main] transmittableThreadLocal: " + transmittableThreadLocal.get());

        // Submit the first task
        executorService.submit(TtlRunnable.get(() -> {
            System.out.println("=====================================================");
            System.out.println("Task 1 - Thread: " + Thread.currentThread());
            System.out.println("[Task 1] inheritableThreadLocal: " + inheritableThreadLocal.get());
            System.out.println("[Task 1] transmittableThreadLocal: " + transmittableThreadLocal.get());
            // Modify the values in the thread
            inheritableThreadLocal.set("Modified Inheritable in Task 1");
            transmittableThreadLocal.set("Modified Transmittable in Task 1");
        }));

        // Update values in the main thread
        inheritableThreadLocal.set("Updated Inheritable in Main");
        transmittableThreadLocal.set("Updated Transmittable in Main");

        // Submit the second task
        executorService.submit(TtlRunnable.get(() -> {
            System.out.println("=====================================================");
            System.out.println("Task 2 - Thread: " + Thread.currentThread());
            System.out.println("[Task 2] inheritableThreadLocal: " + inheritableThreadLocal.get());
            System.out.println("[Task 2] transmittableThreadLocal: " + transmittableThreadLocal.get());
        }));

        executorService.shutdown();
    }
}
