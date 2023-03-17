package com.saidake.test;

public interface MotherFace{
    void motherFunc(double value);
    default void defaultFunc(){}
}
