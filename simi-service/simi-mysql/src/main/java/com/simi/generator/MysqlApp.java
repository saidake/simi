package com.simi.generator;

import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.client.discovery.EnableDiscoveryClient;
import org.springframework.context.ConfigurableApplicationContext;


@SpringBootApplication
@EnableDiscoveryClient
@MapperScan("com.simi.generator.mybatis.mapper")
public class MysqlApp {
    public static void main(String[] args) {
        ConfigurableApplicationContext configurableApplicationContext = SpringApplication.run(MysqlApp.class, args);
//        GeneratorTemplate generatorTemplate = configurableApplicationContext.getBean(GeneratorTemplate.class);
//        GeneratorProperties generatorProperties = configurableApplicationContext.getBean(GeneratorProperties.class);
//        DatabaseMapper databaseMapper= configurableApplicationContext.getBean(DatabaseMapper.class);
//        generatorTemplate.generateCommonVoClass();
    }
}
