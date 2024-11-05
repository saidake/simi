package com.simi.trade.service;

import com.simi.trade.domain.test.TestStudentUpdateRequest;
import com.simi.trade.entity.TestPersonEntity;
import com.simi.trade.repository.TestPersonRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.support.TransactionSynchronizationManager;

@Service
@Slf4j
public class TestTempService {

    @Autowired
    TestPersonRepository testPersonRepository;


    public void updateTestPerson(TestStudentUpdateRequest testStudentUpdateRequest) {
        TestPersonEntity testPerson = testPersonRepository.findById(1l).orElse(null);
        testPerson.setName(testStudentUpdateRequest.getClassName());
        testPersonRepository.save(testPerson);
        log.info( "CurrentTransactionName: {}",TransactionSynchronizationManager.getCurrentTransactionName());
    }
}
