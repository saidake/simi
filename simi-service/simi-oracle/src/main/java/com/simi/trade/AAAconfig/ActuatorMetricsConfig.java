//package com.simi.trade.AAAconfig;
//
//import org.springframework.beans.factory.InitializingBean;
//import org.springframework.beans.factory.config.BeanPostProcessor;
//import org.springframework.context.annotation.Bean;
//import org.springframework.context.annotation.Configuration;
//
//@Configuration
//public class ActuatorMetricsConfig {
//    @Bean
//    InitializingBean forcePrometheusPostProcessor(BeanPostProcessor meterRegistryPostProcessor, PrometheusMeterRegistry registry) {
//        return () -> meterRegistryPostProcessor.postProcessAfterInitialization(registry, "");
//    }
//}
