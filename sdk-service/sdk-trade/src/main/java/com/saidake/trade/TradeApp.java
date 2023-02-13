package com.saidake.trade;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.client.discovery.EnableDiscoveryClient;
import org.springframework.cloud.netflix.eureka.EnableEurekaClient;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.scheduling.annotation.EnableAsync;

import java.util.Arrays;

@SpringBootApplication
@EnableEurekaClient
@EnableDiscoveryClient
@EnableAsync
public class TradeApp {
    public static void main(String[] args){
        ConfigurableApplicationContext run = SpringApplication.run(TradeApp.class, args);
        System.out.println(Arrays.asList(run.getBeanDefinitionNames()));
    }
}