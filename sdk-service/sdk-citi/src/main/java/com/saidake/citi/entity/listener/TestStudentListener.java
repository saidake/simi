package com.saidake.citi.entity.listener;


import com.saidake.citi.entity.TestStudentEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.persistence.PostPersist;
import javax.persistence.PostUpdate;
import javax.servlet.http.HttpServletRequest;
import javax.transaction.Transactional;
import java.util.Objects;

@Slf4j
@Component
public class TestStudentListener {

    @PostPersist
    @Transactional(Transactional.TxType.REQUIRES_NEW)
    public void postPersist(TestStudentEntity testStudentEntity) {
        HttpServletRequest request =( (ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
        log.info("postPersist======================================================================");
        log.info("request url: {}",request.getRequestURI());
        log.info("request query string: {}",request.getQueryString());
        log.info("TestStudentEntity: {}",testStudentEntity);
    }

    @PostUpdate
    @Transactional(Transactional.TxType.REQUIRES_NEW)
    public void postUpdate(TestStudentEntity testStudentEntity) {
        HttpServletRequest request =( (ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
        log.info("postUpdate======================================================================");
        log.info("request url: {}",request.getRequestURI());
        log.info("request query string: {}",request.getQueryString());
        log.info("TestStudentEntity: {}",testStudentEntity);
    }
}