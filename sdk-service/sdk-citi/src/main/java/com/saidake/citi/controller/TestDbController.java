package com.saidake.citi.controller;

import com.saidake.citi.entity.TestStudent;
import com.saidake.citi.repository.TestStudentRepository;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import java.util.List;

@RestController
@Slf4j
@Tag(name = "TestDb")
public class TestDbController {

    @Resource
    TestStudentRepository testStudentRepository;

    @GetMapping("/teststudent")
    @Operation(description = "测试数据接口")
    public List<TestStudent> getTestStudent() {
        return testStudentRepository.findAll();
    }
}
