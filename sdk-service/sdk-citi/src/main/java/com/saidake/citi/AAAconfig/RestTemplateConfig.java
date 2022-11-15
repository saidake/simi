package com.saidake.citi.AAAconfig;

import org.springframework.cloud.client.loadbalancer.LoadBalanced;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.http.converter.xml.Jaxb2RootElementHttpMessageConverter;
import org.springframework.web.client.RestTemplate;

@Configuration
public class RestTemplateConfig {

    @Bean
    @Primary
//    @LoadBalanced
    public RestTemplate restTemplate(){
        setTrustStore();
        final RestTemplate restTemplate=new RestTemplate();
//        restTemplate.getInterceptors().add();
//        restTemplate.getMessageConverters().add(0,new Jaxb2RootElementHttpMessageConverter());
        return restTemplate;
    }

    private void setTrustStore() {

    }

}
