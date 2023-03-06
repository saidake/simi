package com.saidake.generator.AAAconfig;

import org.springframework.cloud.client.loadbalancer.LoadBalanced;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.client.RestTemplate;

@Configuration
public class GlobalConfig {
    @Bean
    @LoadBalanced
        // 负载均衡，Ribbon会拦截
    RestTemplate restTemplate(){
        return new RestTemplate();
    }
}
