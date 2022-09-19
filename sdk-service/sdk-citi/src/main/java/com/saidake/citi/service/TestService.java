package com.saidake.citi.service;

import com.saidake.citi.entity.TestStudentEntity;
import com.saidake.citi.repository.TestStudentRepository;
import com.saidake.common.log.annotation.SysLog;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpServletRequest;
import javax.transaction.Transactional;
import java.util.HashMap;

@Service
@Slf4j
public class TestService {
    @Autowired
    RestTemplate restTemplate;

    @Autowired
    TestStudentRepository testStudentRepository;

    @Transactional
    public void testAsync(Long id) throws InterruptedException {
        RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
        HttpServletRequest request = ((ServletRequestAttributes) requestAttributes).getRequest();
        printToken(request,id);
        requestInstance();
//        saveTestStudent();
        if(id==1){
            throw new RuntimeException("test exception");
        }
    }

    public void testSync(Long id) throws InterruptedException {
        Thread.sleep(3000);
        requestInstance();
//        printToken();
//        saveTestStudent();
        if(id==1){
            throw new RuntimeException("test exception");
        }
    }

    private void saveTestStudent() {
        TestStudentEntity testStudentEntity = new TestStudentEntity();
        testStudentEntity.setClassName("classSync");
        testStudentEntity.setPerId(1L);
        testStudentEntity.setStuId(9L);
        testStudentEntity.setRole("monitor");
        testStudentRepository.save(testStudentEntity);
    }

    public void printToken(HttpServletRequest request,Long id) throws InterruptedException {
        Thread.sleep(6000);
        log.info("=========测试token: {}", request.getHeader("token"));
        log.info("=========测试id: {}", id);
    }
    private void requestInstance() {
        ResponseEntity<String> exchange = restTemplate.exchange("http://SDK-GENERATOR/test", HttpMethod.GET, new HttpEntity<>(new HashMap<>(), new HttpHeaders()), String.class);
        log.info("=========测试响应 response: {}", exchange.getBody());
    }
}
