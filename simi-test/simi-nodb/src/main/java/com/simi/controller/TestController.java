package com.simi.controller;

import com.fasterxml.jackson.annotation.JsonFormat;
import jakarta.servlet.http.HttpServletRequest;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Async;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.CompletableFuture;

@RestController
@Slf4j
public class TestController {


    @Value("${spring.application.name:}")
    private String applicationName;

    /**
     * Base test request.
     *
     * @param id
     * @param str
     * @return
     * @throws TestFirstException
     * @throws TestSecondException
     */
    @GetMapping("/test")
    public String getTest(@RequestParam(required = false) Integer id,
                          @RequestParam(required = false) String str
    ) throws TestFirstException, TestSecondException {
        log.info("id："+id);
        if(id!=null&&id==2){
            throw new TestFirstException();
        }else if(id!=null&&id==3){
            throw new TestSecondException();
        }
        return applicationName +" /test "+ id;
    }

    /**
     * Test post request.
     *
     * @param testRequestBody
     * @return
     * @throws TestFirstException
     * @throws TestSecondException
     */
    @PostMapping("/test")
    public TestRequestBody getTestObj(@RequestBody TestRequestBody testRequestBody) throws TestFirstException, TestSecondException {
        log.info("testRequestBody: {}",testRequestBody);
        return testRequestBody;
    }

    /**
     * Test post request of multipart/form-data type.
     *
     * @param fileName
     * @param file
     * @return
     * @throws TestFirstException
     * @throws TestSecondException
     * @throws IOException
     */
    @PostMapping("/testFormData")
    public String getTestObj(@RequestParam("fileName") String fileName, @RequestPart("file") MultipartFile file) throws TestFirstException, TestSecondException, IOException {
        log.info("fileName: {}",fileName);
        log.info("file: {}",file.getBytes().length);
        log.info("file: {}",file.getContentType());
        return fileName;
    }

    /**
     * Test asynchronous request.
     * @param headers
     * @param id
     * @return
     * @throws InterruptedException
     */
    @GetMapping("/async")
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

    /**
     * Test synchronous request.
     *
     * @param headers
     * @param id
     * @return
     * @throws InterruptedException
     */
    @GetMapping("/sync")
    public String syncSend(@RequestHeader Map<String,String> headers,@RequestParam("id") Long id) throws InterruptedException {
        testSync(id);
        return applicationName +" /sync "+ id;
    }

    //=============================================================================================== Test Method
    private void testSync(Long id) throws InterruptedException {
        RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
        HttpServletRequest request = ((ServletRequestAttributes) requestAttributes).getRequest();
        Thread.sleep(3000);
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

    //=============================================================================================== Test Class
    private static class TestFirstException extends RuntimeException {
    }
    private static class TestSecondException extends RuntimeException {
    }

    @Data
    private static class TestRequestBody {
        @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd HH:mm:ss")
        private Date date;

        private String name;
    }


}
