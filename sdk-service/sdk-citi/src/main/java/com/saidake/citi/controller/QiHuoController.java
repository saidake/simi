package com.saidake.citi.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.*;

@RestController
@Slf4j
@Tag(name = "期货通数据")
public class QiHuoController {
    @Value("${day.sr}")
    private String srDayData;

    @GetMapping("/sr")
    public String getTest() {
        return "success:"+srDayData;
    }
}
