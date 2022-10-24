package com.saidake.citi.service;

import com.saidake.citi.domain.test.TestStudentAddRequest;
import com.saidake.citi.domain.test.TestStudentUpdateRequest;
import com.saidake.citi.entity.TestStudentEntity;
import org.springframework.http.ResponseEntity;

import java.util.List;

public interface TestStudentService  {

    ResponseEntity<List<TestStudentEntity>> listHandler(Integer page, Integer limit);

    ResponseEntity<TestStudentEntity> detailHandler(Long id);

    ResponseEntity<Long> addHandler(TestStudentAddRequest testStudentAddRequest);

    ResponseEntity<Long> updateHandler(TestStudentUpdateRequest testStudentUpdateRequest);

    ResponseEntity<Long> deleteHandler(Long id);
}
