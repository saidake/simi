package com.simi.trade;


import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.client.discovery.EnableDiscoveryClient;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.scheduling.annotation.EnableAsync;

import javax.sql.DataSource;


@SpringBootApplication
@EnableDiscoveryClient
@EnableAsync
public class OracleApp {
    public static void main(String[] args){
        ConfigurableApplicationContext run = SpringApplication.run(OracleApp.class, args);
        DataSource bean = run.getBean(DataSource.class);
        System.out.println(bean);
    }
}