package com.saidake.trade.service;

import com.saidake.trade.entity.TestPersonEntity;
import com.saidake.trade.entity.TestStudentEntity;
import com.saidake.trade.repository.TestPersonRepository;
import jakarta.transaction.TransactionManager;
import jakarta.transaction.Transactional;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.stereotype.Service;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.TransactionSynchronizationManager;


import java.util.List;

@Service
@Slf4j
public class TestPersonService {
    @Autowired
    TestPersonRepository testPersonRepository;
    @Autowired
    MongoOperations mongoOperations;
    @Autowired
    PlatformTransactionManager platformTransactionManager;

    public void test(){
//        log.info("getCurrentTransactionName: {}",TransactionSynchronizationManager.getCurrentTransactionName());
//        log.info("getCurrentTransactionIsolationLevel: {}",TransactionSynchronizationManager.getCurrentTransactionIsolationLevel());
//        log.info("getSynchronizations: {}",TransactionSynchronizationManager.getSynchronizations());
//        log.info("getResourceMap: {}",TransactionSynchronizationManager.getResourceMap());
        TestPersonEntity firstByPerId = testPersonRepository.findFirstByPerId(1L);
        firstByPerId.setName("dingdangdddddfsfsfs");
        List<TestStudentEntity> testStudentEntities = firstByPerId.getTestStudentEntities();
        firstByPerId.setTestStudentEntities(testStudentEntities);
        testPersonRepository.save(firstByPerId);
        System.out.println("success");
    }
}
