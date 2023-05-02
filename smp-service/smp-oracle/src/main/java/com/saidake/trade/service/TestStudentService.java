package com.saidake.trade.service;

import com.saidake.trade.domain.test.TestStudentAddRequest;
import com.saidake.trade.domain.test.TestStudentUpdateRequest;
import com.saidake.trade.entity.TestStudentEntity;
import com.saidake.trade.repository.TestStudentRepository;
import com.saidake.trade.repository.TestStudentSaveLogRepository;
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
