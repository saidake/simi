package com.simi.sandbox.libtest.inheritablethreadlocal;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Callable;

public class SimplifiedTransmittableThreadLocal<T> extends ThreadLocal<T> {

    // Track all instances of this class
    private static final Set<SimplifiedTransmittableThreadLocal<?>> instances = ConcurrentHashMap.newKeySet();

    public SimplifiedTransmittableThreadLocal() {
        instances.add(this);
    }

    @Override
    public void set(T value) {
        super.set(value);
    }

    @Override
    public void remove() {
        super.remove();
    }

    // Capture the current thread's thread-local values
    public static Map<SimplifiedTransmittableThreadLocal<?>, Object> capture() {
        Map<SimplifiedTransmittableThreadLocal<?>, Object> captured = new ConcurrentHashMap<>();
        for (SimplifiedTransmittableThreadLocal<?> threadLocal : instances) {
            captured.put(threadLocal, threadLocal.get());
        }
        return captured;
    }

    // Restore the captured thread-local values to the current thread
    public static void restore(Map<SimplifiedTransmittableThreadLocal<?>, Object> captured) {
        for (SimplifiedTransmittableThreadLocal<?> threadLocal : instances) {
            if (captured.containsKey(threadLocal)) {
                @SuppressWarnings("unchecked")
                SimplifiedTransmittableThreadLocal<Object> castedThreadLocal =
                        (SimplifiedTransmittableThreadLocal<Object>) threadLocal;
                castedThreadLocal.set(captured.get(threadLocal));
            } else {
                threadLocal.remove(); // Remove if not present in captured map
            }
        }
    }

}
