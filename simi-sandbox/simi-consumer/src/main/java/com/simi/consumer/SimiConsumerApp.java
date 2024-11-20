package com.simi.consumer;


import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.scheduling.annotation.EnableScheduling;

@SpringBootApplication
@EnableScheduling
public class SimiConsumerApp {
    public static void main(String[] args){
        ConfigurableApplicationContext run = SpringApplication.run(SimiConsumerApp.class, args);
        //LambdaTemplate bean = run.getBean(LambdaTemplate.class);
        //System.out.println(bean.invokeLambdaFunction("SimiLambdaFunction","{'functionName': 'greet', 'data': 'Alice'}"));
    }
}
