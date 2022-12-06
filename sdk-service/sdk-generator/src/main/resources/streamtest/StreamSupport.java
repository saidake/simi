package com.saidake.streamtest;

import com.saidake.backup.Spliterator;

import java.util.Objects;


public final class StreamSupport {
    public static <T> Stream<T> stream(Spliterator<T> spliterator, boolean parallel) {
        Objects.requireNonNull(spliterator);
        return new ReferencePipeline.Head<>(spliterator,
                StreamOpFlag.fromCharacteristics(spliterator),
                parallel);
    }
}
