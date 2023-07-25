package com.simi.trade.service;

import com.simi.trade.domain.test.TestStudentAddRequest;
import com.simi.trade.domain.test.TestStudentUpdateRequest;
import com.simi.trade.entity.TestStudentEntity;
import com.simi.trade.repository.TestStudentRepository;
import com.simi.trade.repository.TestStudentSaveLogRepository;
import jakarta.annotation.Resource;
import jakarta.transaction.Transactional;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@Slf4j
public class TestStudentService  {

    @Resource
    private TestStudentRepository testStudentRepository;

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
        log.info("query started");
        TestStudentEntity testStudentEntity = testStudentRepository.findFirstByStuId(testStudentUpdateRequest.getStuId());
        BeanUtils.copyProperties(testStudentUpdateRequest,testStudentEntity);
        log.info("save started");
        testStudentRepository.save(testStudentEntity);
        log.info("save successfully");
        return ResponseEntity.ok(null);
    }

    public ResponseEntity deleteHandler(Long id) {
        testStudentRepository.deleteById(id);
        return ResponseEntity.ok(null);
    }

}
