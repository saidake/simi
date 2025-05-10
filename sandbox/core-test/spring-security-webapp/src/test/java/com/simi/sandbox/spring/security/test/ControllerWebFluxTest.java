package com.simi.sandbox.spring.security.test;

import com.simi.sandbox.spring.security.SpringSecurityApp;
import com.simi.sandbox.spring.security.test.AAAConfig.WebSecurityConfig;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
import org.springframework.test.web.reactive.server.WebTestClient;


@Import(WebSecurityConfig.class)
@SpringBootTest(classes = SpringSecurityApp.class, webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Slf4j
class ControllerWebFluxTest {

    @Autowired
    private WebTestClient webTestClient;

    @Test
    void testPing() {
        var result = webTestClient.get()
                .uri("/ping")
                .exchange()
                .expectStatus().is5xxServerError()
                .returnResult(String.class);

        // Print status, headers, and body
        log.info("Status: {}", result.getStatus());
        System.out.println("test");
        log.info("Headers: {}", result.getResponseHeaders());
        result.getResponseBody()
                .collectList()
                .doOnNext(body -> System.out.println("Body: " + body))
                .block(); // block() is safe here for test/debug logging

    }
}