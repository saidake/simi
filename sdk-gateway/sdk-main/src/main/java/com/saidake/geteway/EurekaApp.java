package com.saidake.geteway;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.client.discovery.EnableDiscoveryClient;
import org.springframework.context.ConfigurableApplicationContext;

@EnableDiscoveryClient
@SpringBootApplication
public class EurekaApp {
    public static void main(String[] args) {
        ConfigurableApplicationContext run = SpringApplication.run(EurekaApp.class, args);
//        Docket bean = run.getBean(Docket.class);
//        System.out.println("bean"+bean);
//        System.out.println(bean);
    }
}
