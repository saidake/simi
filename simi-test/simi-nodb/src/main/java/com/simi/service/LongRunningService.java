package com.simi.service;

import lombok.extern.log4j.Log4j;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.util.concurrent.CompletableFuture;

@Service
@Slf4j
public class LongRunningService {

    @Async("customExecutor")
    public CompletableFuture<String> performLongRunningTask() {
        log.info("start calculating");
        long time=System.currentTimeMillis();
        int sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }
        sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }
        sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }
        sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }
        sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }
        sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }
        sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }
        sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }
        sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }
        sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }
        sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }
        sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }
        sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }
        sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }
        sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }
        sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }
        sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }
        sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }
        sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }
        sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }
        sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }
        sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }
        sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }
        sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }
        sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }
        sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }
        sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }
        sum=0;
        for (int i = 0; i < 1000_000_000; i++) {
            sum+=i;
        }

        log.info("stop calculating: "+(System.currentTimeMillis()-time)/1000);
        return CompletableFuture.completedFuture("dingdingding");
    }
}
