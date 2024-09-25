package com.simi.webflux.AAAconfiguration;


import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.function.Function;

@Configuration
public class AmazonLambdaConfiguration {
    @Bean
    public Function<String, String> greet() {
        return name -> "Hello, " + name + "!";
    }

    @Bean
    public Function<Integer, String> square() {
        return number -> "Square: " + (number * number);
    }

}
