package com.simi.sandbox.libtest.inheritablethreadlocal;


import com.alibaba.ttl.TransmittableThreadLocal;

import java.util.WeakHashMap;

/**
 * A simplified version of {@link TransmittableThreadLocal}.
 */
public class SimiInheritableThreadLocal<T> extends InheritableThreadLocal<T> {

    private static final InheritableThreadLocal<WeakHashMap<SimiInheritableThreadLocal<Object>, ?>> holder =
            new InheritableThreadLocal<WeakHashMap<SimiInheritableThreadLocal<Object>, ?>>() {
                @Override
                protected WeakHashMap<SimiInheritableThreadLocal<Object>, ?> initialValue() {
                    return new WeakHashMap<SimiInheritableThreadLocal<Object>, Object>();
                }

                @Override
                protected WeakHashMap<SimiInheritableThreadLocal<Object>, ?> childValue(WeakHashMap<SimiInheritableThreadLocal<Object>, ?> parentValue) {
                    return new WeakHashMap<SimiInheritableThreadLocal<Object>, Object>(parentValue);
                }
            };

    private final boolean disableIgnoreNullValueSemantics;

    public SimiInheritableThreadLocal(boolean disableIgnoreNullValueSemantics) {
        this.disableIgnoreNullValueSemantics = disableIgnoreNullValueSemantics;
    }

    @Override
    public final void set(T value) {
        if (!disableIgnoreNullValueSemantics && null == value) {
            // may set null to remove value
            remove();
        } else {
            super.set(value);
            addThisToHolder();
        }
    }

    private void addThisToHolder() {
        if (!holder.get().containsKey(this)) {
            holder.get().put((SimiInheritableThreadLocal<Object>) this, null); // WeakHashMap supports null value.
        }
    }


    @Override
    public final void remove() {
        holder.get().remove(this);
        super.remove();
    }

}
