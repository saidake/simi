package com.saidake.streamtest;

import java.util.Comparator;

public interface Spliterator<T> {
    int CONCURRENT = 0x00001000;
    int SIZED      = 0x00000040;
    int SUBSIZED = 0x00004000;
    int SORTED     = 0x00000004;
    long estimateSize();
    Spliterator<T> trySplit();
    int characteristics();
    default Comparator<? super T> getComparator() {
        throw new IllegalStateException();
    }
    default boolean hasCharacteristics(int characteristics) {
        return (characteristics() & characteristics) == characteristics;
    }
}
