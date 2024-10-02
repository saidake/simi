package com.simi.webflux;


import com.simi.webflux.domain.TestPerson;
import com.simi.webflux.service.TestPersonService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;

import java.util.List;

@Slf4j
@SpringBootApplication
public class SimiWebFluxApp {
    public static void main(String[] args){
        ConfigurableApplicationContext run = SpringApplication.run(SimiWebFluxApp.class, args);
//        TestPersonService testPersonService = run.getBean(TestPersonService.class);
//        List<TestPerson> allTestPersons = testPersonService.getAllTestPersons();
//        System.out.println(allTestPersons);
    }
}
