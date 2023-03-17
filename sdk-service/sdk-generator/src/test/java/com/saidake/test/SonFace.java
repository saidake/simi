package com.saidake.test;

public interface SonFace<T, Z> extends FatherFace<T, Z> {
    int CC=77;
    void accept(T t);
    default void sonDefault(){
    }
    interface SonInsideFace extends SonFace<Double,Integer>, MotherFace {
        @Override
        default void accept(Double i) {
        }
        @Override
        default void defaultFunc() {
        }
    }
}