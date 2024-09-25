package com.simi.streamtest;

import java.util.function.Consumer;
import java.util.function.Function;

public interface Stream<T> extends BaseStream<T, Stream<T>>  {
    <R> Stream<R> map(Function<? super T, ? extends R> mapper);
    <R, A> R collect(Collector<? super T, A, R> collector);
    void forEach(Consumer<? super T> action);

}
