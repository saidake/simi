package com.saidake.citi.controller;

import com.saidake.citi.service.TestService;
import com.saidake.common.log.annotation.SysLog;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
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

    @GetMapping("/async")
    @SysLog("asyncSend")
    @Operation(description = "测试异步接口")
    public String asyncSend(@RequestHeader Map<String,String> headers,@RequestParam("id") Long id) throws InterruptedException {
        testService.testAsync(id);
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
}
