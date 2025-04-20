package com.simi.sandbox.common.log.controller;

import com.simi.common.log.annotation.SysLog;
import org.springframework.stereotype.Service;

@Service
public class TestService {

    @SysLog
    public void logTest(){
        int a=3/0;
    }
}
