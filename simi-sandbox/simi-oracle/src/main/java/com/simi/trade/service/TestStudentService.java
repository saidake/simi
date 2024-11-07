package com.simi.trade.service;

import com.simi.trade.domain.test.TestStudentAddRequest;
import com.simi.trade.domain.test.TestStudentUpdateRequest;
import com.simi.trade.entity.StudentRoleEnum;
import com.simi.trade.entity.TestPersonEntity;
import com.simi.trade.entity.TestStudentEntity;
import com.simi.trade.repository.TestPersonRepository;
import com.simi.trade.repository.TestStudentRepository;
import com.simi.trade.repository.TestStudentSaveLogRepository;
import jakarta.annotation.Resource;
import jakarta.transaction.Transactional;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.util.List;
import java.util.Optional;

@Service
@Slf4j
public class TestStudentService  {
    @Autowired
    private TestTempService testTempService;

    @Resource
    private TestStudentRepository testStudentRepository;
    @Resource
    private TestPersonRepository testPersonRepository;
    @Resource
    private TestStudentSaveLogRepository testStudentSaveLogRepository;

    public ResponseEntity<List<TestStudentEntity>> listHandler(Integer page, Integer limit) {
        testStudentRepository.findFirstByStuId(5L);
        TestStudentEntity byStuIdOrRole1 = testStudentRepository.findByStuIdOrRole(4L, "leader", PageRequest.of(0,1)).get(0);
        TestStudentEntity byStuIdOrRole2= testStudentRepository.findByStuIdOrRole(3L, "monitor", PageRequest.of(0,1)).get(0);
        TestStudentEntity byStuIdOrRole3 = testStudentRepository.findFirstByStuIdOrRole(4L, "leader");
        TestStudentEntity byStuIdOrRole4= testStudentRepository.findFirstByStuIdOrRole(3L, "monitor");
        return ResponseEntity.ok(testStudentRepository.findAll(PageRequest.of(page,limit)).getContent());
    }

    public ResponseEntity<TestStudentEntity> detailHandler(Long id) {
        return ResponseEntity.ok(testStudentRepository.findById(id).orElse(null));
    }

    public ResponseEntity<Long> addHandler(TestStudentAddRequest testStudentAddRequest) {
        TestStudentEntity testStudentEntity=new TestStudentEntity();
        testStudentAddRequest.setClassName(testStudentAddRequest.getClassName().length()>30?testStudentAddRequest.getClassName().substring(0,30):testStudentAddRequest.getClassName());
        BeanUtils.copyProperties(testStudentAddRequest,testStudentEntity);
        testStudentRepository.save(testStudentEntity);
        return ResponseEntity.ok(testStudentEntity.getStuId());
    }


    @Transactional
    public ResponseEntity<Long> updateHandler(TestStudentUpdateRequest testStudentUpdateRequest) {
        TestPersonEntity testPerson2 = testPersonRepository.findById(1l).orElse(null);

        //A. update TEST_STUDENT
        TestStudentEntity testStudentEntity = testStudentRepository.findFirstByStuId(testStudentUpdateRequest.getStuId());
        testStudentEntity.setClassName(testStudentUpdateRequest.getClassName());
        testStudentRepository.save(testStudentEntity);

        //A. add TEST_STUDENT
        TestStudentEntity testStudentEntityNew = new TestStudentEntity();
        testStudentEntityNew.setClassName(testStudentUpdateRequest.getClassName()+"new");
        testStudentEntityNew.setPerId(1l);
        testStudentRepository.save(testStudentEntityNew);
        TestPersonEntity testPerson = testPersonRepository.findById(1l).orElse(null);
        testPerson.getTestStudentEntities().add(testStudentEntityNew);
        log.info( "CurrentTransactionName: {}",TransactionSynchronizationManager.getCurrentTransactionName());

        //A. update TEST_PERSON
        testPerson2.setName(testStudentUpdateRequest.getClassName());
        testPersonRepository.save(testPerson2);
        TestPersonEntity testPerson3 = testPersonRepository.findById(1l).orElse(null);
        log.info( "CurrentTransactionName: {}",TransactionSynchronizationManager.getCurrentTransactionName());
        return ResponseEntity.ok(null);
    }



    @Transactional
    public ResponseEntity<Long> updateHandlerTransactional(TestStudentUpdateRequest testStudentUpdateRequest) {
        //A. update TEST_STUDENT
        TestStudentEntity testStudentEntity = testStudentRepository.findFirstByStuId(testStudentUpdateRequest.getStuId());
        testStudentEntity.setClassName(testStudentUpdateRequest.getClassName());
        testStudentRepository.save(testStudentEntity);
        log.info( "CurrentTransactionName: {}",TransactionSynchronizationManager.getCurrentTransactionName());
        //A. update TEST_PERSON
        try{
            testTempService.updateTestPerson(testStudentUpdateRequest);
        }catch (Exception ex){
            ex.printStackTrace();
            log.info("error!!");
        }
        //A. update TEST_STUDENT2
        TestStudentEntity testStudentEntity2 = testStudentRepository.findFirstByStuId(testStudentUpdateRequest.getStuId());
        testStudentEntity2.setClassName(testStudentUpdateRequest.getClassName()+"ddd");
        testStudentRepository.save(testStudentEntity2);
        System.out.println(1/0);
        return ResponseEntity.ok(null);
    }



    public ResponseEntity deleteHandler(Long id) {
        testStudentRepository.deleteById(id);
        return ResponseEntity.ok(null);
    }


    @Transactional
    public ResponseEntity<String> deleteByPerId(Long perId) {
        testStudentRepository.deleteById(4L);
        testPersonRepository.deleteById(perId);
        return ResponseEntity.ok("success");
    }
}
