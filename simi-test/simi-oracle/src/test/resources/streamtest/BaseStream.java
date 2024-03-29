package com.simi.streamtest;

public interface BaseStream<T, S extends BaseStream<T, S>>
        extends AutoCloseable {
    boolean isParallel();

}
