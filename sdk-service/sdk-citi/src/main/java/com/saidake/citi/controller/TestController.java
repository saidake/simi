package com.saidake.citi.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;

@RestController
@Slf4j
@Tag(name = "Test")
public class TestController {

    @GetMapping("/test")
    public String getTest(@RequestParam(required = false) String id) {
        log.info("id："+id);
        return "success233cc3:"+id;
    }

    @PostMapping("/multi")
    public String getPostTest(@RequestBody Object test) {
        return "success"+test;
    }
}
