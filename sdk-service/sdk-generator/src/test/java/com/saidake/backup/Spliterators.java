//package com.saidake.backup;
//
//
//import com.saidake.backup.Spliterator;
//
//import java.util.Collection;
//import java.util.Iterator;
//import java.util.Objects;
//
//public final class Spliterators {
//
//    public static <T> Spliterator<T> spliterator(Collection<? extends T> c,
//                                                 int characteristics) {
//        return new IteratorSpliterator<>(Objects.requireNonNull(c),
//                characteristics);
//    }
//
//    static class IteratorSpliterator<T> implements Spliterator<T> {
//        private final Collection<? extends T> collection; // null OK
//        private Iterator<? extends T> it;
//        private final int characteristics;
//
//        public IteratorSpliterator(Collection<? extends T> collection, int characteristics) {
//            this.collection = collection;
//            this.it = null;
//            this.characteristics = (characteristics & Spliterator.CONCURRENT) == 0
//                    ? characteristics | Spliterator.SIZED | Spliterator.SUBSIZED
//                    : characteristics;
//        }
//        @Override
//        public int characteristics() { return characteristics; }
//    }
//}
