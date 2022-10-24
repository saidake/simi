package com.saidake.citi.domain.qihuo;


import com.alibaba.excel.annotation.ExcelProperty;
import lombok.Data;

@Data
public class AccountInfoExcel {
    //====================================================================== 标准信息
    @ExcelProperty("Name")
    private String name;

    @ExcelProperty("Code")
    private String code;

    @ExcelProperty("Std Point")
    private String stdPoint;

    @ExcelProperty("Point Value")
    private String pointValue;

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
    @ExcelProperty("Enter Price")
    private String enterPrice;

    @ExcelProperty("Out Price")
    private String outPrice;

    @ExcelProperty("Trade Point")
    private String tradePoint;

    @ExcelProperty("Net Profit")
    private String netProfit;

    @ExcelProperty("Enter Assets")
    private String enterAssets;

    @ExcelProperty("Out Assets")
    private String outAssets;

    @ExcelProperty("Real Profit")
    private String realProfit;

    @ExcelProperty("Calc Profit")
    private String CalcProfit;

    //====================================================================== 交易系统
    @ExcelProperty("Rule")
    private String rule;

    @ExcelProperty("Trade Reason")
    private String tradeReason;

    @ExcelProperty("Fail Reason")
    private String failReason;
}
