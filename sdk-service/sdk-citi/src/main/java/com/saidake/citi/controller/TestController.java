package com.saidake.citi.controller;

import com.saidake.citi.domain.test.TestRequestBody;
import com.saidake.citi.service.TestService;
import com.saidake.common.log.annotation.SysLog;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

@RestController
@Slf4j
@Tag(name = "Test")
public class TestController {

    @Autowired
    TestService testService;

    @GetMapping("/test")
    @Operation(description = "测试普通接口")
    public String getTest(@RequestParam(required = false) Integer id) throws TestFirstException, TestSecondException {
        log.info("id："+id);
        if(id==2){
            throw new TestFirstException();
        }else if(id==3){
            throw new TestSecondException();
        }
        return "success233cc3:"+id;
    }

    @PostMapping("/test")
    @Operation(description = "测试普通对象接口")
    public TestRequestBody getTestObj(@RequestBody TestRequestBody testRequestBody) throws TestFirstException, TestSecondException {
        log.info("testRequestBody: {}",testRequestBody);
        return testRequestBody;
    }
    @GetMapping("/async")
    @SysLog("asyncSend")
    @Operation(description = "测试异步接口")
    public String asyncSend(@RequestHeader Map<String,String> headers,@RequestParam("id") Long id) throws InterruptedException {
        testAsync(id);
        log.info("test token: {}",headers.get("token"));
        return "success-async";
    }

    @GetMapping("/sync")
    @SysLog("syncSend")
    @Operation(description = "测试同步接口")
    public String syncSend(@RequestHeader Map<String,String> headers,@RequestParam("id") Long id) throws InterruptedException {
        testService.testSync(id);
        log.info("test token: {}",headers.get("token"));
        return "success-async";
    }



    //    @Transactional
    @Async
//    @SuppressWarnings("unchecked")
    public void testAsync(Long id) throws InterruptedException {
        RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
        HttpServletRequest request = ((ServletRequestAttributes) requestAttributes).getRequest();
        testService.printToken(request,id);
        testService.requestInstance();
        if(id==1){
            throw new RuntimeException("test exception");
        }
    }

}
