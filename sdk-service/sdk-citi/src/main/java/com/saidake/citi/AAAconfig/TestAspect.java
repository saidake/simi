//package com.saidake.citi.AAAconfig;
//
//import lombok.SneakyThrows;
//import lombok.extern.slf4j.Slf4j;
//import org.aspectj.lang.ProceedingJoinPoint;
//import org.aspectj.lang.annotation.Around;
//import org.aspectj.lang.annotation.Aspect;
//import org.aspectj.lang.annotation.Before;
//import org.springframework.stereotype.Component;
//
//@Aspect
//@Slf4j
//@Component
//public class TestAspect {
//
//    @Around("execution(* com.saidake.citi.controller.*.* (*)) && !@annotation(com.saidake.common.log.annotation.SysLog)")
//    @SneakyThrows
//    public Object aroundTest(ProceedingJoinPoint point) {
//        System.out.println("test");
//        Object proceed = point.proceed();
//        return proceed;
//    }
//
//}
