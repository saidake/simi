package com.simi.toolkit.core.exception;

import lombok.NoArgsConstructor;

@NoArgsConstructor
public class InitException extends Exception{
    public InitException(String message) {
        super(message);
    }
}
