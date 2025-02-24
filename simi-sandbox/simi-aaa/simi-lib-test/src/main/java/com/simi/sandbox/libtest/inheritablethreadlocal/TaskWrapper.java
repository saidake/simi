package com.simi.sandbox.libtest.inheritablethreadlocal;

import java.util.Map;
import java.util.concurrent.Callable;

public class TaskWrapper {

    public static Runnable wrap(Runnable task) {
        Map<SimplifiedTransmittableThreadLocal<?>, Object> captured = SimplifiedTransmittableThreadLocal.capture();
        return () -> {
            Map<SimplifiedTransmittableThreadLocal<?>, Object> previous = SimplifiedTransmittableThreadLocal.capture();
            try {
                SimplifiedTransmittableThreadLocal.restore(captured);
                task.run();
            } finally {
                SimplifiedTransmittableThreadLocal.restore(previous); // Restore the original values
            }
        };
    }

    public static <T> Callable<T> wrap(Callable<T> task) {
        Map<SimplifiedTransmittableThreadLocal<?>, Object> captured = SimplifiedTransmittableThreadLocal.capture();
        return () -> {
            Map<SimplifiedTransmittableThreadLocal<?>, Object> previous = SimplifiedTransmittableThreadLocal.capture();
            try {
                SimplifiedTransmittableThreadLocal.restore(captured);
                return task.call();
            } finally {
                SimplifiedTransmittableThreadLocal.restore(previous); // Restore the original values
            }
        };
    }
}
