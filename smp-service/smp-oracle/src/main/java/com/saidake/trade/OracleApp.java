package com.saidake.trade;

import com.saidake.common.log.aspect.SysLogAspect;
import com.saidake.trade.entity.TestPersonEntity;
import com.saidake.trade.entity.TestStudentEntity;
import com.saidake.trade.repository.TestPersonRepository;
import com.saidake.trade.service.TestPersonService;
import jakarta.transaction.Transactional;
import org.springdoc.core.models.GroupedOpenApi;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.client.discovery.EnableDiscoveryClient;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.aggregation.Aggregation;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.scheduling.annotation.EnableAsync;

import java.util.List;


@SpringBootApplication
@EnableDiscoveryClient
@EnableAsync
public class OracleApp {
    public static void main(String[] args){
        ConfigurableApplicationContext run = SpringApplication.run(OracleApp.class, args);
//        SysLogAspect bean = run.getBean(SysLogAspect.class);
//        System.out.println("AAAAAAAAAAAAAAAAAAA: "+bean);
//        TestPersonService testPersonService = run.getBean(TestPersonService.class);
//        testPersonService.test();
//        System.out.println("success");
    }
}