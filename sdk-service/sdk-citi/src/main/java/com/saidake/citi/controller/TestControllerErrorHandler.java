package com.saidake.citi.controller;

import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.context.request.NativeWebRequest;


@ControllerAdvice
@ResponseBody
@Slf4j
public class TestControllerErrorHandler {

    @ExceptionHandler(TestFirstException.class)
    public String processControllerError1(NativeWebRequest request, TestFirstException ex) {
        System.out.println("TestFirstException");
        return "TestFirstException";
    }

    @ExceptionHandler(TestSecondException.class)
    public String processControllerError2(NativeWebRequest request, TestSecondException ex) {
        System.out.println("TestSecondException");
        return "TestSecondException";
    }

//    @ExceptionHandler(Throwable.class)
//    public String processControllerError3(NativeWebRequest request, Throwable ex) {
//        System.out.println("throwable: "+ex.getMessage());
//        return "Throwable";
//    }

}

