package com.simi.service.trade.controller;

import com.simi.service.trade.domain.AccountResponse;
import com.simi.service.trade.service.FxcmService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.xml.sax.SAXException;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPathExpressionException;
import java.io.*;
@RestController

@Tag(name = "FXCM")
@Slf4j
@RequestMapping("fxcm")
public class FxcmController {
    @Autowired
    private FxcmService fxcmService;
    @GetMapping("/account")
    @Operation(description = "Retrieve FXCM account information")
    public ResponseEntity<AccountResponse> getAccountInfo() throws IOException, SAXException, ParserConfigurationException, XPathExpressionException {
        return ResponseEntity.ok(fxcmService.retrieveFxcmAccountInfo());
    }
}
