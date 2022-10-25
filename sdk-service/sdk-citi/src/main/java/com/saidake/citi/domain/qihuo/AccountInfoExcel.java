package com.saidake.citi.domain.qihuo;


import com.alibaba.excel.annotation.ExcelProperty;
import lombok.Data;

@Data
public class AccountInfoExcel {
    //====================================================================== 标准信息
    @ExcelProperty("Account Type")
    private String accountType;

    @ExcelProperty("Name")
    private String name;

    @ExcelProperty("Code")
    private String code;

    @ExcelProperty("Std Point")
    private String stdPoint;

    @ExcelProperty("Point Value")
    private String pointValue;

    @ExcelProperty("Num")
    private String num;

    @ExcelProperty("Cost")
    private String cost;

    @ExcelProperty("Type")
    private String type;
    //====================================================================== 趋势信息
    @ExcelProperty("Trend Time")
    private String trendTime;

    @ExcelProperty("Trend Direction")
    private String trendDirection;

    //====================================================================== 交易信息
    @ExcelProperty("Trade Time")
    private String tradeTime;

    @ExcelProperty("Trade Direction")
    private String tradeDirection;

    @ExcelProperty("Except Point")
    private Integer exceptPoint;

    @ExcelProperty("Enter Price")
    private String enterPrice;

    @ExcelProperty("Out Price")
    private String outPrice;

    @ExcelProperty("Trade Point")
    private String tradePoint;

    @ExcelProperty("Net Profit")
    private String netProfit;

    //====================================================================== 交易系统
    @ExcelProperty("Rule")
    private String rule;

    @ExcelProperty("Reference Line")
    private String referenceLine;

    @ExcelProperty("Trade Reason")
    private String tradeReason;

    @ExcelProperty("Second Reason")
    private String secondReason;

    @ExcelProperty("Fail Reason")
    private String failReason;

    //====================================================================== 额外信息
    @ExcelProperty("Enter Assets")
    private String enterAssets;

    @ExcelProperty("Out Assets")
    private String outAssets;

    @ExcelProperty("Real Profit")
    private String realProfit;

    @ExcelProperty("Calc Profit")
    private String CalcProfit;
}
