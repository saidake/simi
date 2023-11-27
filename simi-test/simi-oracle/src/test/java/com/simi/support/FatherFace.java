package com.simi.support;

public interface FatherFace<T, Z>{
    void fatherFunc(double size);
    default void defaultFunc(){}
}