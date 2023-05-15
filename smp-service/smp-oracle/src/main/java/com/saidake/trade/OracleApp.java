package com.saidake.trade;

import com.saidake.common.log.aspect.SysLogAspect;
import com.saidake.trade.domain.test.DingDong;
import com.saidake.trade.domain.test.Person;
import com.saidake.trade.entity.TestPersonEntity;
import com.saidake.trade.entity.TestStudentEntity;
import com.saidake.trade.repository.TestPersonRepository;
import com.saidake.trade.repository.TestStudentRepository;
import com.saidake.trade.service.TestPersonService;
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

import java.util.List;


@SpringBootApplication
@EnableDiscoveryClient
@EnableAsync
public class OracleApp {
    public static void main(String[] args){
        ConfigurableApplicationContext run = SpringApplication.run(OracleApp.class, args);
//        DozerBeanMapper dozerBeanMapper = run.getBean(DozerBeanMapper.class);
//        Person person = new Person();
//        person.setAge(23);
//        person.setName("dd");
//        DingDong dingDong = new DingDong();
//        dingDong.setAge(99);
//        dingDong.setTt("ttddd");
//        DingDong map = dozerBeanMapper.map(person, DingDong.class,"dingdong");
//        System.out.println(map);

        TestStudentRepository testStudentRepository = run.getBean(TestStudentRepository.class);
        testStudentRepository.findFirstByStuId(5L);
//        TestStudentEntity byStuIdOrRole3 = testStudentRepository.findFirstByStuIdOrRoleNative(5L, "normal");
        TestStudentEntity byStuIdOrRole1 = testStudentRepository.findByStuIdOrRole(4L, "leader", PageRequest.of(0,1)).get(0);
        TestStudentEntity byStuIdOrRole2= testStudentRepository.findByStuIdOrRole(3L, "monitor", PageRequest.of(0,1)).get(0);
        TestStudentEntity byStuIdOrRole3 = testStudentRepository.findFirstByStuIdOrRole(4L, "leader");
        TestStudentEntity byStuIdOrRole4= testStudentRepository.findFirstByStuIdOrRole(3L, "monitor");

    }
}