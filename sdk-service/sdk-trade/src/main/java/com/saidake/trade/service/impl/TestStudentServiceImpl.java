package com.saidake.trade.service.impl;

import com.saidake.trade.domain.test.TestStudentAddRequest;
import com.saidake.trade.domain.test.TestStudentUpdateRequest;
import com.saidake.trade.entity.TestStudentEntity;
import com.saidake.trade.repository.TestStudentRepository;
import com.saidake.trade.repository.TestStudentSaveLogRepository;
import com.saidake.trade.service.TestStudentService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.List;

@Slf4j
@Service
public class TestStudentServiceImpl implements TestStudentService {

    @Resource
    private TestStudentRepository testStudentRepository;

    @Resource
    private TestStudentSaveLogRepository testStudentSaveLogRepository;

    @Override
    public ResponseEntity<List<TestStudentEntity>> listHandler(Integer page, Integer limit) {
        return ResponseEntity.ok(testStudentRepository.findAll(PageRequest.of(page,limit)).getContent());
    }

    @Override
    public ResponseEntity<TestStudentEntity> detailHandler(Long id) {
        return ResponseEntity.ok(testStudentRepository.findById(id).orElse(null));
    }

    @Override
    public ResponseEntity<Long> addHandler(TestStudentAddRequest testStudentAddRequest) {
        TestStudentEntity testStudentEntity=new TestStudentEntity();
        testStudentAddRequest.setClassName(testStudentAddRequest.getClassName().length()>30?testStudentAddRequest.getClassName().substring(0,30):testStudentAddRequest.getClassName());
        BeanUtils.copyProperties(testStudentAddRequest,testStudentEntity);
        testStudentRepository.save(testStudentEntity);
        return ResponseEntity.ok(testStudentEntity.getStuId());
    }

    @Override
    public ResponseEntity<Long> updateHandler(TestStudentUpdateRequest testStudentUpdateRequest) {
        TestStudentEntity testStudentEntity = testStudentRepository.findFirstByStuId(testStudentUpdateRequest.getStuId());
        BeanUtils.copyProperties(testStudentUpdateRequest,testStudentEntity);
        testStudentRepository.save(testStudentEntity);
        return ResponseEntity.ok(null);
    }

    @Override
    public ResponseEntity deleteHandler(Long id) {
        testStudentRepository.deleteById(id);
        return ResponseEntity.ok(null);
    }

}
