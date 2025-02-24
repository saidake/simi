package com.simi.sandbox.common.log.controller;

import jakarta.servlet.http.HttpServletRequest;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

@RestController
@Slf4j
public class TestController {
    @Autowired
    TestService testService;

    @GetMapping("/testGet")
    public ResponseEntity<String> getTest(@RequestParam(required = false) Integer id) throws TestFirstException, TestSecondException {
        log.info("id："+id);
        if(id!=null&&id==1){
            throw new TestFirstException();
        }else if(id!=null&&id==2){
            throw new TestSecondException();
        }
        testService.logTest();
        return ResponseEntity.ok("success");
    }

    @PostMapping("/testPost")
    public ResponseEntity<TestRequestBody> getTestObj(@RequestBody TestRequestBody testRequestBody){
        log.info("testRequestBody: {}",testRequestBody);
        return ResponseEntity.ok(testRequestBody);
    }

    @PostMapping("/testFormData")
    public ResponseEntity<String> getTestObj(@RequestParam("fileName") String fileName, @RequestPart("file") MultipartFile file) throws IOException {
        log.info("fileName: {}",fileName);
        log.info("file: {}",String.valueOf(file.getBytes().length));
        log.info("file: {}",file.getContentType());
        return ResponseEntity.ok(fileName);
    }

    @GetMapping("/testAsync")
    public ResponseEntity<String> asyncSend(@RequestHeader Map<String,String> headers, @RequestParam("id") Long id) throws InterruptedException {
        log.info("started");
        List<CompletableFuture<String>> completableFutures=new ArrayList<>();
        for (int i = 0; i < 50; i++) {
            completableFutures.add(CompletableFuture.supplyAsync(()->{
                log.info("Async logic");
                return "success";
            }));
        }
        CompletableFuture.allOf(completableFutures.toArray(new CompletableFuture[0])).join();
        return ResponseEntity.ok("success");
    }


    @GetMapping("/testTPP")
    public ResponseEntity<String> syncSend(@RequestHeader Map<String,String> headers, @RequestParam("id") Long id) {
        RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
        HttpServletRequest request = ((ServletRequestAttributes) requestAttributes).getRequest();
        return ResponseEntity.ok("success");
    }

    //=============================================================================================== Test Class
    private static class TestFirstException extends RuntimeException {
    }
    private static class TestSecondException extends RuntimeException {
    }

    @Data
    public static class TestRequestBody {
        private Date date;
        private String name;
    }

}

