package com.simi.consumer;


import com.simi.consumer.AAAConfig.LambdaTemplate;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;

@SpringBootApplication
public class LambdaConsumerApp {
    public static void main(String[] args){
        ConfigurableApplicationContext run = SpringApplication.run(LambdaConsumerApp.class, args);
        LambdaTemplate bean = run.getBean(LambdaTemplate.class);
        System.out.println(bean.invokeLambdaFunction("SimiLambdaFunction","{'functionName': 'greet', 'data': 'Alice'}"));
    }
}
