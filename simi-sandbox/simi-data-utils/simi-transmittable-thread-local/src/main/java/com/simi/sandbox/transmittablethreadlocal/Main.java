package com.simi.sandbox.transmittablethreadlocal;

import com.alibaba.ttl.TransmittableThreadLocal;
import com.simi.common.util.thread.SimiInheritableThreadLocal;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class Main {
    public static ThreadLocal<String> threadLocal=ThreadLocal.withInitial(()->"init");
    public static InheritableThreadLocal<String> inheritableThreadLocal=new InheritableThreadLocal<>();
    public static TransmittableThreadLocal<String> transmittableThreadLocal=new TransmittableThreadLocal<>();
    public static SimiInheritableThreadLocal<String> simiInheritableThreadLocal=new SimiInheritableThreadLocal<>();
    public static void main(String[] args) {
        executorServiceTest();
//        transmittableThreadLocalTest();
    }

    private static void transmittableThreadLocalTest() {
        ExecutorService executorService = Executors.newFixedThreadPool(1);
        threadLocal.set("Parent");
        inheritableThreadLocal.set("Inheritable Parent");
        transmittableThreadLocal.set("Transmittable Parent");
        simiInheritableThreadLocal.set("Transmittable Parent");

        System.out.println("[Parent] threadLocal: " + threadLocal.get());
        System.out.println("[Parent] inheritableThreadLocal: " + inheritableThreadLocal.get());
        System.out.println("[Parent] transmittableThreadLocal: " + transmittableThreadLocal.get());
        System.out.println("[Parent] simiInheritableThreadLocal: " + simiInheritableThreadLocal.get());
        System.out.println("=====================================================");

        CompletableFuture<Void> completableFuture1= CompletableFuture.runAsync(()->{
            // Task thread inherits the ThreadLocal value
            System.out.println("[Child1] threadLocal: " + threadLocal.get());
            System.out.println("[Child1] inheritableThreadLocal: " + inheritableThreadLocal.get());
            System.out.println("[Child1] transmittableThreadLocal: " + transmittableThreadLocal.get());
            System.out.println("[Child1] simiInheritableThreadLocal: " + simiInheritableThreadLocal.get());
            threadLocal.set("Child1");
            inheritableThreadLocal.set("Child1");
            transmittableThreadLocal.set("Child1");
            simiInheritableThreadLocal.set("Child1");
        },executorService);
        completableFuture1.join();
        System.out.println("=====================================================");

        CompletableFuture<Void> completableFuture2= CompletableFuture.runAsync(()->{
            System.out.println("[Child2] threadLocal: " + threadLocal.get());
            System.out.println("[Child2] inheritableThreadLocal: " + inheritableThreadLocal.get());
            System.out.println("[Child2] transmittableThreadLocal: " + transmittableThreadLocal.get());
            System.out.println("[Child2] simiInheritableThreadLocal: " + simiInheritableThreadLocal.get());
        },executorService);
        completableFuture2.join();
    }

    private static void executorServiceTest() {
        ExecutorService executorService = Executors.newFixedThreadPool(1);
        threadLocal.set("Parent");
        inheritableThreadLocal.set("Inheritable Parent");
        transmittableThreadLocal.set("Transmittable Parent");
        System.out.println("=====================================================");
        System.out.println("Thread Name: " + Thread.currentThread());
        System.out.println("[Parent] threadLocal: " + threadLocal.get());
        System.out.println("[Parent] inheritableThreadLocal: " + inheritableThreadLocal.get());
        System.out.println("[Parent] transmittableThreadLocal: " + transmittableThreadLocal.get());

        executorService.submit(() -> {
            System.out.println("=====================================================");
            System.out.println("Thread Name: " + Thread.currentThread());
            System.out.println("[Child1] threadLocal: " + threadLocal.get());
            System.out.println("[Child1] inheritableThreadLocal: " + inheritableThreadLocal.get());
            System.out.println("[Child1] transmittableThreadLocal: " + transmittableThreadLocal.get());
            threadLocal.set("Child1");
            inheritableThreadLocal.set("Child1");
            transmittableThreadLocal.set("Child1");
        });

        executorService.submit(() -> {
            System.out.println("=====================================================");
            System.out.println("Thread Name: " + Thread.currentThread());
            System.out.println("[Child2] threadLocal: " + threadLocal.get());
            System.out.println("[Child2] inheritableThreadLocal: " + inheritableThreadLocal.get());
            System.out.println("[Child2] transmittableThreadLocal: " + transmittableThreadLocal.get());
        });
        executorService.shutdown();
    }
}
