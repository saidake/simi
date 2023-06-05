package com.saidake.trade.AAAconfig;


import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.context.annotation.Scope;
import org.springframework.context.annotation.ScopedProxyMode;
import org.springframework.stereotype.Component;
import org.springframework.web.context.WebApplicationContext;

@Component
@Scope(value=WebApplicationContext.SCOPE_REQUEST, proxyMode= ScopedProxyMode.TARGET_CLASS)
@Slf4j
public class FormBuilder implements DisposableBean {

    public void logInfo(){
        log.info("FormBuilder logInfo");
    }
    @Override
    public void destroy() throws Exception {
      log.info("FormBuilder has been destroyed");
    }
}
