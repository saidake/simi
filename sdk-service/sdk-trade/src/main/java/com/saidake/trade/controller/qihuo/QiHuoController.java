package com.saidake.trade.controller.qihuo;

import com.alibaba.excel.EasyExcel;
import com.saidake.trade.domain.qihuo.AccountInfoExcel;
import com.saidake.trade.domain.qihuo.AccountInfoExcelListener;
import com.saidake.trade.domain.qihuo.QihuoAccountInfoResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.*;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;

@RestController
@Slf4j
@Tag(name = "期货通数据")
@RequestMapping("/qihuo")
public class QiHuoController {

    @Value("${qihuo.simulate.account-info-path}")
    private String qihuoAccountInfoPath;

    @GetMapping("/account")
    public QihuoAccountInfoResponse getAccount() {
        AccountInfoExcelListener accountInfoExcelListener = new AccountInfoExcelListener();
        try(BufferedInputStream bufferedInputStream=new BufferedInputStream(new FileInputStream(qihuoAccountInfoPath))){
            EasyExcel.read(bufferedInputStream, AccountInfoExcel.class,accountInfoExcelListener).sheet().doRead();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return accountInfoExcelListener.getQihuoAccountInfoResponse();
    }
}