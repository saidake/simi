//package com.simi.webflux.AAAconfig;
//
//import com.simi.webflux.service.TestMessageProducerService;
//import lombok.RequiredArgsConstructor;
//import org.springframework.boot.CommandLineRunner;
//import org.springframework.stereotype.Component;
//
//@Component
//@RequiredArgsConstructor
//public class SnsPublisherRunner implements CommandLineRunner {
//
//    private final TestMessageProducerService snsService;
//
//    @Override
//    public void run(String... args) {
//        String message = "Hello from Spring Boot!";
//        String topicArn = "arn:aws:sns:us-east-1:123456789012:my-topic";
//        snsService.publishToTopic(message, topicArn);
//    }
//}
