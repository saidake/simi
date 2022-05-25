package com.saidake.mybatis.plus;


import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.client.discovery.EnableDiscoveryClient;

@SpringBootApplication
@EnableDiscoveryClient
public class MybatisPlusApp {
    public static void main(String[] args){
        SpringApplication.run(MybatisPlusApp.class,args);
    }
}
