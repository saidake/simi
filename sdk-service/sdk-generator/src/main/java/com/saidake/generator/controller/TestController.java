package com.saidake.generator.controller;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;

@RestController
@Slf4j
@Api(tags = "Swagger测试接口", value = "swagger")
@RequestMapping
public class TestController {
    @GetMapping("/test")
    @ApiOperation(value = "列表")
    public String getTest( ) {
        return "success";
    }

    @GetMapping("/fafa")
    @ApiOperation(value = "dd")
    public HashMap<String, Long> getDDTest( @RequestParam(required = false) @ApiParam(value = "ID", example = "0") String id) {
        log.info("id："+id);
        HashMap<String, Long> objectObjectHashMap = new HashMap<String, Long>(){{
            put("date",1662494400000L);
        }};
        return objectObjectHashMap;
    }
}
