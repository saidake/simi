package com.simi.common.util.file.support;

import lombok.NoArgsConstructor;

@NoArgsConstructor
public class InitException extends Exception{
    public InitException(String message) {
        super(message);
    }
}
