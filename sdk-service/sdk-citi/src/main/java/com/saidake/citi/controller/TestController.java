package com.saidake.citi.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;

@RestController
@Slf4j
@Tag(name = "Test")
public class TestController {

    @GetMapping("/test")
    @Operation(description = "测试接口")
    public String getTest(@RequestParam(required = false) String id) {
        log.info("id："+id);
        return "success233cc3:"+id;
    }

    @PostMapping("/multi")
    public String getPostTest(@RequestPart MultipartFile file,
                              @RequestParam(value = "docName",required = false) String docName,
                              @RequestParam(value = "userName",required = false) String userName, HttpServletRequest request) throws InterruptedException {
        Thread.sleep(120000);
        return "success"+docName;
    }
}
