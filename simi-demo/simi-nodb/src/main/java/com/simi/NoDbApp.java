package com.simi;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.web.servlet.ServletComponentScan;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.scheduling.annotation.EnableAsync;

@SpringBootApplication
@ServletComponentScan
@EnableAsync
public class NoDbApp {
    public static void main(String[] args) {
        ConfigurableApplicationContext run = SpringApplication.run(NoDbApp.class, args);
    }
}