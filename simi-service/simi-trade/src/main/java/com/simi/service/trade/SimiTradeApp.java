package com.simi.service.trade;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.scheduling.annotation.EnableAsync;

@SpringBootApplication
@EnableAsync
public class SimiTradeApp {
    public static void main(String[] args){
        ConfigurableApplicationContext run = SpringApplication.run(SimiTradeApp.class, args);
    }
}