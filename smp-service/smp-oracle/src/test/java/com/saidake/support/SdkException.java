package com.saidake.support;

public class SdkException extends RuntimeException{
    SdkException(Exception e){
        super(e);
    }
}
