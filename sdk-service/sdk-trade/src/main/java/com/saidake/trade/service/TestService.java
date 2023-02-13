package com.saidake.trade.service;

import com.saidake.trade.repository.TestStudentRepository;
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
import java.util.HashMap;

@Service
@Slf4j
public class TestService {
    @Autowired
    RestTemplate restTemplate;

    @Autowired
    TestStudentRepository testStudentRepository;

//    @Transactional
    @Async
//    @SuppressWarnings("unchecked")
    public void testAsync(Long id) throws InterruptedException {
        RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
        HttpServletRequest request = ((ServletRequestAttributes) requestAttributes).getRequest();
        printToken(request,id);
        requestInstance();
        if(id==1){
            throw new RuntimeException("test exception");
        }
    }

    public void testSync(Long id) throws InterruptedException {
        RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
        HttpServletRequest request = ((ServletRequestAttributes) requestAttributes).getRequest();
        Thread.sleep(3000);
        requestInstance();
        printToken(request,id);
        if(id==1){
            throw new RuntimeException("test exception");
        }
    }


    public void printToken(HttpServletRequest request,Long id) throws InterruptedException {
        Thread.sleep(6000);
        log.info("=========测试token: {}", request.getHeader("token"));
        log.info("=========测试id: {}", id);
    }
    public void requestInstance() {
        ResponseEntity<String> exchange = restTemplate.exchange("http://SDK-GENERATOR/test", HttpMethod.GET, new HttpEntity<>(new HashMap<>(), new HttpHeaders()), String.class);
        log.info("=========测试响应 response: {}", exchange.getBody());
    }
}
