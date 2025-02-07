package com.simi.sandbox.libtest;

import com.alibaba.ttl.TransmittableThreadLocal;
import com.alibaba.ttl.TtlRunnable;
import com.simi.common.util.thread.SimiInheritableThreadLocal;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class ThreadLocalTest {
    public static ThreadLocal<String> threadLocal=ThreadLocal.withInitial(()->"init");
    public static InheritableThreadLocal<String> inheritableThreadLocal=new InheritableThreadLocal<>();
    public static TransmittableThreadLocal<String> transmittableThreadLocal=new TransmittableThreadLocal<>();
    public static SimiInheritableThreadLocal<String> simiInheritableThreadLocal=new SimiInheritableThreadLocal<>();

    public static void main(String[] args) {
        ExecutorService executorService = Executors.newFixedThreadPool(1);
        threadLocal.set("Parent1");
        inheritableThreadLocal.set("Inheritable Parent1");
        transmittableThreadLocal.set("Transmittable Parent1");
        simiInheritableThreadLocal.set("SimiInheritable Parent1");

        //----------------------------------------------------------------------------- Propagate parent thread value
        executorService.submit(()->{
            System.out.println("===================================================== Propagate parent thread value");
            System.out.println("threadLocal: " + threadLocal.get());
            System.out.println("inheritableThreadLocal: " + inheritableThreadLocal.get());
            System.out.println("transmittableThreadLocal: " + transmittableThreadLocal.get());
            System.out.println("simiInheritableThreadLocal: " + simiInheritableThreadLocal.get());
        });
        threadLocal.set("Parent2");
        inheritableThreadLocal.set("Inheritable Parent2");
        transmittableThreadLocal.set("Transmittable Parent2");
        simiInheritableThreadLocal.set("SimiInheritable Parent2");
        //----------------------------------------------------------------------------- Modified parent thread value
        executorService.submit(()->{
            System.out.println("===================================================== Modified parent thread value");
            System.out.println("threadLocal: " + threadLocal.get());
            System.out.println("inheritableThreadLocal: " + inheritableThreadLocal.get());
            threadLocal.set("Modified Parent2");
            inheritableThreadLocal.set("Modified Inheritable Parent2");
            transmittableThreadLocal.set("Modified Transmittable Parent2");
        });
        executorService.submit(TtlRunnable.get(()->{
            System.out.println("transmittableThreadLocal: " + transmittableThreadLocal.get());
            transmittableThreadLocal.set("Modified Transmittable Parent2");
        }));
        executorService.submit(()->{
            System.out.println("simiInheritableThreadLocal: " + simiInheritableThreadLocal.get());
            simiInheritableThreadLocal.set("Modified SimiInheritable Parent2");
        });
        //----------------------------------------------------------------------------- Modified child thread value
        executorService.submit(()->{
            System.out.println("===================================================== Modified child thread value");
            System.out.println("threadLocal: " + threadLocal.get());
            System.out.println("inheritableThreadLocal: " + inheritableThreadLocal.get());
        },executorService);

        executorService.submit(TtlRunnable.get(()->{
            System.out.println("transmittableThreadLocal: " + transmittableThreadLocal.get());
        }));
        executorService.submit(()->{
            System.out.println("simiInheritableThreadLocal: " + simiInheritableThreadLocal.get());
        });
        executorService.shutdown();
    }
}
