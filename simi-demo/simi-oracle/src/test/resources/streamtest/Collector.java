package com.simi.streamtest;

import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.Supplier;

public interface Collector<T, A, R>  {

    Set<Characteristics> characteristics();
    enum Characteristics {
        CONCURRENT,
        UNORDERED,
        IDENTITY_FINISH
    }
    BiConsumer<A, T> accumulator();
    Function<A, R> finisher();
    Supplier<A> supplier();

}
