package com.saidake.citi.service.impl;

import com.saidake.citi.domain.test.TestStudentAddRequest;
import com.saidake.citi.domain.test.TestStudentUpdateRequest;
import com.saidake.citi.entity.TestStudentEntity;
import com.saidake.citi.repository.TestStudentRepository;
import com.saidake.citi.repository.TestStudentSaveLogRepository;
import com.saidake.citi.service.TestStudentService;
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
        BeanUtils.copyProperties(testStudentAddRequest,testStudentEntity);
        testStudentSaveLogRepository.save(testStudentEntity);
        return ResponseEntity.ok(testStudentEntity.getStuId());
    }

    @Override
    public ResponseEntity<Long> updateHandler(TestStudentUpdateRequest testStudentUpdateRequest) {
        TestStudentEntity testStudentEntity = testStudentRepository.findFirstByStuId(testStudentUpdateRequest.getStuId());
        BeanUtils.copyProperties(testStudentUpdateRequest,testStudentEntity);
        testStudentSaveLogRepository.save(testStudentEntity);
        return ResponseEntity.ok(null);
    }

    @Override
    public ResponseEntity deleteHandler(Long id) {
        testStudentRepository.deleteById(id);
        return ResponseEntity.ok(null);
    }

}
