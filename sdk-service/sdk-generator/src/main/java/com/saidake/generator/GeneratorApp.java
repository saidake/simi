package com.saidake.generator;

import com.saidake.generator.AAAconfig.properties.GeneratorProperties;
import com.saidake.generator.mybatis.mapper.DatabaseMapper;
import com.saidake.generator.service.generator.GeneratorService;
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
        GeneratorService generatorService = configurableApplicationContext.getBean(GeneratorService.class);
        GeneratorProperties generatorProperties = configurableApplicationContext.getBean(GeneratorProperties.class);
        DatabaseMapper databaseMapper= configurableApplicationContext.getBean(DatabaseMapper.class);
//        generatorProperties.getServices().forEach(item->{
//            List<ServiceFieldsConfig> addHandler = item.getAddHandler();
//            System.out.println(addHandler);
//        });
//        generatorService.initParams();
//        generatorService.clearOtherFiles();
//        generatorService.generateTemplateFiles();
//        System.out.println(generatorProperties.getParams());
//        generatorService.generateTemplateFiles();
//        generatorService.generateCommonVoClassFiles();
        generatorService.generateAppendServicesFiles();
    }
}
