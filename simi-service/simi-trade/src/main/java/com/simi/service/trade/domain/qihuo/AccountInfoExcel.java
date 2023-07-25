package com.simi.service.trade.domain.qihuo;


import com.alibaba.excel.annotation.ExcelProperty;
import lombok.Data;

@Data
public class AccountInfoExcel {
    //====================================================================== 标准信息
    @ExcelProperty("账号类型")
    private String accountType;

    @ExcelProperty("名称")
    private String name;

    @ExcelProperty("代码")
    private String code;

    @ExcelProperty("类型")
    private String type;
    //====================================================================== 交易信息
    @ExcelProperty("本金")
    private String baseAssets;

    @ExcelProperty("交易时间")
    private String tradeTime;

    @ExcelProperty("交易方向")
    private String tradeDirection;

    @ExcelProperty("手数")
    private String num;

    @ExcelProperty("期望点数")
    private Integer exceptPoint;

    @ExcelProperty("开单价格")
    private String enterPrice;

    @ExcelProperty("结算价格")
    private String outPrice;

    @ExcelProperty("结算点数")
    private String tradePoint;

    @ExcelProperty("净盈利")
    private String netProfit;

    //====================================================================== 交易系统
    @ExcelProperty("规则")
    private String rule;

    @ExcelProperty("参考线")
    private String referenceLine;

    @ExcelProperty("交易原因")
    private String tradeReason;

    @ExcelProperty("次要原因")
    private String secondReason;

    @ExcelProperty("风险")
    private String risk;

    @ExcelProperty("失败原因")
    private String failReason;

}
