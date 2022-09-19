package com.saidake.generator.controller;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@Slf4j
@Api(tags = "Swagger测试接口", value = "swagger")
public class TestController {
    @GetMapping("/test")
    @ApiOperation(value = "列表")
    public String getTest( ) {
        return "success";
    }
    @GetMapping("/fafa")
    @ApiOperation(value = "dd")
    public String getDDTest( @RequestParam(required = false) @ApiParam(value = "ID", example = "0") String id) {
        log.info("id："+id);
        return "success+fafa";
    }

}
