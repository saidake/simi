package com.saidake.geteway.AAAconfig;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.context.config.annotation.RefreshScope;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@Slf4j
@RefreshScope
public class TestController {
    @GetMapping("/test")
    public String getTest(@RequestParam(required = false) String id) {
        log.info("id："+id);
        return "success";
    }
}
