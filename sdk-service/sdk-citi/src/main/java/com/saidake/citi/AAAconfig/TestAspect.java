package com.saidake.citi.AAAconfig;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.stereotype.Component;

@Aspect
@Slf4j
@Component
public class TestAspect {

    @Around("execution(* com.saidake.citi.controller.*.* (*))")
    @SneakyThrows
    public Object aroundTest(ProceedingJoinPoint point) {
        System.out.println("TestAspect");
        try{
            return point.proceed();
        }catch (Exception ex ){
            System.out.println("TestAspect throw");
            throw ex;
        }
    }

}
