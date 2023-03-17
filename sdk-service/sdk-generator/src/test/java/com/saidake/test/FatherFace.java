package com.saidake.test;

public interface FatherFace<T, Z>{
    void fatherFunc(double size);
    default void defaultFunc(){}
}