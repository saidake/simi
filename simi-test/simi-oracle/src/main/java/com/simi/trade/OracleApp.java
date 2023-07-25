package com.simi.trade;

import com.simi.common.log.aspect.SysLogAspect;
import jakarta.transaction.Transactional;
import net.bytebuddy.build.HashCodeAndEqualsPlugin;
import org.dozer.DozerBeanMapper;
import org.springdoc.core.models.GroupedOpenApi;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.client.discovery.EnableDiscoveryClient;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.aggregation.Aggregation;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.scheduling.annotation.EnableAsync;

import javax.sql.DataSource;


@SpringBootApplication
@EnableDiscoveryClient
@EnableAsync
public class OracleApp {
    public static void main(String[] args){
        ConfigurableApplicationContext run = SpringApplication.run(OracleApp.class, args);
        DataSource bean = run.getBean(DataSource.class);
        System.out.println(bean);
    }
}