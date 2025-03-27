package com.simi.aaa.spring.security.test.controller;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.MediaType;
import org.springframework.http.codec.multipart.FilePart;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.IntStream;

@RestController
@Slf4j
public class TestControllerWebFlux {

    @GetMapping("/testGet")
    public Mono<String> getTest(@RequestParam(required = false) Integer id) {
        log.info("id: {}", id);
        if (id != null && id == 1) {
            return Mono.error(new TestFirstException());
        } else if (id != null && id == 2) {
            return Mono.error(new TestSecondException());
        }
        return Mono.just("success");
    }

    @PostMapping("/testPost")
    public Mono<TestRequestBody> getTestObj(@RequestBody Mono<TestRequestBody> testRequestBody) {
        return testRequestBody.doOnNext(req -> log.info("testRequestBody: {}", req));
    }

    @PostMapping(value = "/testFormData", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public Mono<String> getTestObj(@RequestPart("fileName") String fileName, @RequestPart("file") Mono<FilePart> file) {
        return file.flatMap(filePart -> filePart.content().map(dataBuffer -> dataBuffer.toString(StandardCharsets.UTF_8)).then())
                .thenReturn(fileName)
                .doOnNext(name -> log.info("fileName: {}", name));
    }

    @GetMapping("/testAsync")
    public Mono<String> asyncSend(@RequestHeader Map<String, String> headers, @RequestParam("id") Long id) {
        log.info("started");
        List<Mono<String>> monoList = IntStream.range(0, 50)
                .mapToObj(i -> Mono.fromSupplier(() -> {
                    log.info("Async logic");
                    return "success";
                }))
                .toList();

        return Flux.concat(monoList).then(Mono.just("success"));
    }

    @GetMapping("/testTPP")
    public Mono<String> syncSend(ServerWebExchange exchange, @RequestParam("id") Long id) {
        return Mono.just("success");
    }

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