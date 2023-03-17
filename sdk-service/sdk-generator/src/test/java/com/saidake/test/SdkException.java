package com.saidake.test;

public class SdkException extends RuntimeException{
    SdkException(Exception e){
        super(e);
    }
}
