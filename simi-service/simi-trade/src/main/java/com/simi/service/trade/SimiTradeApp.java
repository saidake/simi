package com.simi.service.trade;

import com.simi.service.trade.service.FxcmService;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.scheduling.annotation.EnableAsync;
import org.xml.sax.SAXException;

import javax.xml.parsers.ParserConfigurationException;
import java.io.IOException;

@SpringBootApplication
@EnableAsync
public class SimiTradeApp {
    public static void main(String[] args) throws IOException, ParserConfigurationException, SAXException {
        ConfigurableApplicationContext run = SpringApplication.run(SimiTradeApp.class, args);
        FxcmService fxcmService = run.getBean(FxcmService.class);
        fxcmService.getFxcmAccountInfo();
    }
}