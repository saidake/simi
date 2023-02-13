package com.saidake.trade.service;

import com.saidake.trade.domain.test.TestStudentAddRequest;
import com.saidake.trade.domain.test.TestStudentUpdateRequest;
import com.saidake.trade.entity.TestStudentEntity;
import org.springframework.http.ResponseEntity;

import java.util.List;

public interface TestStudentService  {

    ResponseEntity<List<TestStudentEntity>> listHandler(Integer page, Integer limit);

    ResponseEntity<TestStudentEntity> detailHandler(Long id);

    ResponseEntity<Long> addHandler(TestStudentAddRequest testStudentAddRequest);

    ResponseEntity<Long> updateHandler(TestStudentUpdateRequest testStudentUpdateRequest);

    ResponseEntity<Long> deleteHandler(Long id);
}
