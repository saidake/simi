package com.saidake.citi;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.client.discovery.EnableDiscoveryClient;
import org.springframework.cloud.netflix.eureka.EnableEurekaClient;

@SpringBootApplication
@EnableEurekaClient           //引导类添加自动装配注解
@EnableDiscoveryClient        //激活DiscoveryClient，Consumer服务通过从EurekaServer中抓取Provider的地址完成远程调用
public class CitiApp {
    public static void main(String[] args){
        SpringApplication.run(CitiApp.class,args);
    }
}