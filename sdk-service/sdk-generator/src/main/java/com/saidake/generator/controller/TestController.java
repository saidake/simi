package com.saidake.generator.controller;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.saidake.common.log.annotation.SysLog;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpServletRequest;
import java.util.*;
import java.util.concurrent.CompletableFuture;

@RestController
@Slf4j
@Tag(name = "Test")
public class TestController {

    @Autowired
    RestTemplate restTemplate;

    @Value("${spring.application.name:}")
    private String applicationName;

    //=============================================================================================== 请求方法
    @GetMapping("/test")
    @Operation(description = "测试普通接口")
    public String getTest(@RequestParam(required = false) Integer id,
                          @RequestParam(required = false) String str
    ) throws TestFirstException, TestSecondException {
        log.info("id："+id);
        if(id!=null&&id==2){
            throw new TestFirstException();
        }else if(id!=null&&id==3){
            throw new TestSecondException();
        }
//        requestOtherService();
        return applicationName +" /test "+ id;
    }

    @PostMapping("/test")
    @Operation(description = "测试普通对象接口")
    public TestRequestBody getTestObj(@RequestBody TestRequestBody testRequestBody) throws TestFirstException, TestSecondException {
        log.info("testRequestBody: {}",testRequestBody);
//        requestOtherService();
        return testRequestBody;
    }
    @GetMapping("/async")
    @SysLog("asyncSend")
    @Operation(description = "测试异步接口")
    public String asyncSend(@RequestHeader Map<String,String> headers,@RequestParam("id") Long id) throws InterruptedException {
        CompletableFuture<Long> longCompletableFuture1 = testAsync1(id);
        CompletableFuture<Long> longCompletableFuture2 = testAsync2(id);
        Object join = longCompletableFuture1.thenCombine(longCompletableFuture2,(a,b)->{
            System.out.println(a+b);
            return "ddddddd";
        }).join();
        System.out.println(join);
        log.info("test token: {}",headers.get("token"));
        return applicationName +" /async "+ id;
    }

    @GetMapping("/sync")
    @SysLog("syncSend")
    @Operation(description = "测试同步接口")
    public String syncSend(@RequestHeader Map<String,String> headers,@RequestParam("id") Long id) throws InterruptedException {
        testSync(id);
//        requestOtherService();
        return applicationName +" /sync "+ id;
    }
    //=============================================================================================== 服务方法
    private void testSync(Long id) throws InterruptedException {
        RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
        HttpServletRequest request = ((ServletRequestAttributes) requestAttributes).getRequest();
        Thread.sleep(3000);
//        requestOtherService();
        printToken(request,id);
        if(id==1){
            throw new RuntimeException("test exception");
        }
    }

    @Async
    public CompletableFuture<Long> testAsync1(Long id) throws InterruptedException {
        RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
        HttpServletRequest request = ((ServletRequestAttributes) requestAttributes).getRequest();
        printToken(request,id);
//        requestOtherService();
        if(id==1){
            throw new RuntimeException("test exception1");
        }
        return CompletableFuture.completedFuture(id+777);
    }
    @Async
    public CompletableFuture<Long> testAsync2(Long id) throws InterruptedException {
        RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
        HttpServletRequest request = ((ServletRequestAttributes) requestAttributes).getRequest();
        printToken(request,id);
//        requestOtherService();
        if(id==1){
            throw new RuntimeException("test exception2");
        }
        return CompletableFuture.completedFuture(id+888);
    }

    private void printToken(HttpServletRequest request,Long id) throws InterruptedException {
        Thread.sleep(6000);
        log.info("=========测试token: {}", request.getHeader("token"));
        log.info("=========测试id: {}", id);
    }

    private void requestOtherService() {
        ResponseEntity<String> exchange = restTemplate.exchange("http://sdk-trade/test", HttpMethod.GET, new HttpEntity<>(new HashMap<>(), new HttpHeaders()), String.class);
        log.info("=========测试响应 response: {}", exchange.getBody());
    }
    //=============================================================================================== 测试类
    private class TestFirstException extends RuntimeException {
    }
    private class TestSecondException extends RuntimeException {
    }

    @Data
    @Schema(name = "test request body")
    private class TestRequestBody {
        //    @JsonFormat(shape = JsonFormat.Shape.STRING,pattern = "MM/dd/yyyy HH:mm:ss")
        @JsonFormat(shape = JsonFormat.Shape.STRING,pattern = "yyyy-MM-dd HH:mm:ss")
        @Schema(type = "string",example = "2022-02-01 16:53:33")
        private Date date;

        @Schema(example = "zhangsan")
        private String name;
    }


}
