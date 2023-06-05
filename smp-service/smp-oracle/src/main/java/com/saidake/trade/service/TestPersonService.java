package com.saidake.trade.service;

import com.saidake.common.core.util.data.RandomUtil;
import com.saidake.trade.entity.StudentRoleEnum;
import com.saidake.trade.entity.TestPersonEntity;
import com.saidake.trade.entity.TestStudentEntity;
import com.saidake.trade.repository.TestPersonRepository;
import com.saidake.trade.repository.TestStudentRepository;
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

    @Autowired
    TestStudentRepository testStudentRepository;
    public void test(){
//        log.info("getCurrentTransactionName: {}",TransactionSynchronizationManager.getCurrentTransactionName());
//        log.info("getCurrentTransactionIsolationLevel: {}",TransactionSynchronizationManager.getCurrentTransactionIsolationLevel());
//        log.info("getSynchronizations: {}",TransactionSynchronizationManager.getSynchronizations());
//        log.info("getResourceMap: {}",TransactionSynchronizationManager.getResourceMap());
        TestPersonEntity firstByPerId = testPersonRepository.findFirstByPerId(1L);
        List<TestStudentEntity> testStudentEntities = firstByPerId.getTestStudentEntities();
        System.out.println(testStudentEntities);
        System.out.println(testStudentEntities.size());
        firstByPerId.setName("test david");
        System.out.println("first test line");
        TestStudentEntity testStudentEntity = new TestStudentEntity();
        testStudentEntity.setPerId(firstByPerId.getPerId());
        testStudentEntity.setRole(StudentRoleEnum.MONITOR);
        testStudentEntity.setClassName("lala");
        //testStudentEntity.setStuId(RandomUtil.getRandomLong(1000L,20000L));
        List<TestStudentEntity> testStudentEntities1 = firstByPerId.getTestStudentEntities();
        testStudentEntities1.add(testStudentEntity);
        testStudentRepository.save(testStudentEntity);
        testPersonRepository.save(firstByPerId);
        List<TestStudentEntity> testStudentEntities2 = firstByPerId.getTestStudentEntities();
        System.out.println(testStudentEntities2);
        System.out.println(testStudentEntities2.size());
        System.out.println("second test line");
    }
    public void testSave(){
        TestPersonEntity firstByPerId = testPersonRepository.findFirstByPerId(1L);
        List<TestStudentEntity> testStudentEntities = firstByPerId.getTestStudentEntities();
        System.out.println(testStudentEntities);
        System.out.println(testStudentEntities.size());
        firstByPerId.setName("test david");
        testPersonRepository.save(firstByPerId);
        System.out.println("first test line");
        TestStudentEntity testStudentEntity = new TestStudentEntity();
        testStudentEntity.setPerId(firstByPerId.getPerId());
        testStudentEntity.setRole(StudentRoleEnum.NORMAL);
        testStudentEntity.setClassName("lala");
        testStudentEntity.setStuId(RandomUtil.getRandomLong(1000L,20000L));
        testStudentRepository.save(testStudentEntity);
        List<TestStudentEntity> testStudentEntities2 = firstByPerId.getTestStudentEntities();
        System.out.println(testStudentEntities2);
        System.out.println(testStudentEntities2.size());
        System.out.println("second test line");
    }
}
