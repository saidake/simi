//package com.saidake.streamtest;
//
//import java.util.Objects;
//import java.util.function.BiFunction;
//import java.util.function.BinaryOperator;
//
//final class ReduceOps  {
//    public static <T, U> TerminalOp<T, U> makeRef(U seed, BiFunction<U, ? super T, U> reducer, BinaryOperator<U> combiner) {
//        Objects.requireNonNull(reducer);
//        Objects.requireNonNull(combiner);
//        class ReducingSink extends Box<U> implements AccumulatingSink<T, U, ReducingSink> {
//            @Override
//            public void begin(long size) {
//                state = seed;
//            }
//
//            @Override
//            public void accept(T t) {
//                state = reducer.apply(state, t);
//            }
//
//            @Override
//            public void combine(ReducingSink other) {
//                state = combiner.apply(state, other.state);
//            }
//        }
//        return new ReduceOp<T, U, ReducingSink>(StreamShape.REFERENCE) {
//            @Override
//            public ReducingSink makeSink() {
//                return new ReducingSink();
//            }
//        };
//    }
//}
