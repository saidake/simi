package com.saidake.generator;

import com.saidake.generator.AAAconfig.properties.GeneratorProperties;
import com.saidake.generator.mybatis.mapper.DatabaseMapper;
import com.saidake.generator.service.generator.GeneratorTemplate;
import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.client.discovery.EnableDiscoveryClient;
import org.springframework.context.ConfigurableApplicationContext;


@SpringBootApplication
@EnableDiscoveryClient
@MapperScan("com.saidake.generator.mybatis.mapper")
public class GeneratorApp {
    public static void main(String[] args) {
        ConfigurableApplicationContext configurableApplicationContext = SpringApplication.run(GeneratorApp.class, args);
        GeneratorTemplate generatorTemplate = configurableApplicationContext.getBean(GeneratorTemplate.class);
        GeneratorProperties generatorProperties = configurableApplicationContext.getBean(GeneratorProperties.class);
        DatabaseMapper databaseMapper= configurableApplicationContext.getBean(DatabaseMapper.class);
//        generatorTemplate.generateCommonVoClass();
    }
}
