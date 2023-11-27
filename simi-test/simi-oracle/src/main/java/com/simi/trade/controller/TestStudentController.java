package com.simi.trade.controller;

import com.simi.trade.domain.test.TestStudentAddRequest;
import com.simi.trade.domain.test.TestStudentUpdateRequest;
import com.simi.trade.entity.TestStudentEntity;
import com.simi.trade.service.TestStudentService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import jakarta.annotation.Resource;
import jakarta.validation.Valid;
import java.util.List;

@RestController
@Slf4j
@RequestMapping("/testStudent")
@Tag(name = "test student controller")
public class TestStudentController {

    @Resource
    TestStudentService testStudentService;

    @GetMapping
    @Operation(description = "列表")
    public ResponseEntity<List<TestStudentEntity>> listRequest(
            @RequestParam(required = false, value = "page") @Parameter(description = "第几页", example = "0") Integer page,
            @RequestParam(required = false, value = "limit") @Parameter(description = "每页大小", example = "10") Integer limit) {
        return testStudentService.listHandler(page,limit);
    }

    @GetMapping("/{id}")
    @Operation(description = "查询")

    public ResponseEntity<TestStudentEntity> detailRequest(@PathVariable Long id) {
        return testStudentService.detailHandler(id);
    }

    @Operation(description = "新增")
    @PostMapping
    public ResponseEntity addRequest(@RequestBody @Parameter(description = "json请求体，参数对应其每个属性") @Valid TestStudentAddRequest testStudentAddRequest) {
        return testStudentService.addHandler(testStudentAddRequest);
    }

    @Operation(description = "更新")
    @PutMapping
    public ResponseEntity updateRequest(@RequestBody @Parameter(description = "json请求体，参数对应其每个属性") @Valid TestStudentUpdateRequest testStudentUpdateRequest) {
        return testStudentService.updateHandler(testStudentUpdateRequest);
    }

    @Operation(description = "删除")
    @DeleteMapping("/{id}")
    public ResponseEntity deleteRequest(@PathVariable Long id) {
        return testStudentService.deleteHandler(id);
    }

    @Operation(description = "Association Deletion")
    @DeleteMapping("/ass/{id}")
    public ResponseEntity<String> deleteAssociation(@PathVariable Long id) {
        return testStudentService.deleteByPerId(id);
    }
}
