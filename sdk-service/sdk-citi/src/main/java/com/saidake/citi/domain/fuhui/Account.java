package com.saidake.citi.domain.fuhui;

import lombok.Data;

@Data
public class Account {
    String currencyType;

    //开始时间：2022/7/14 7:26
    String startTime;

    //结束时间：2022/7/14 7:26
    String endTime;

    //成交量：1000
    String turnover;


    //买进：1.0007200000000001
    String purchasePrice;

    //卖出：1.0007200000000001
    String sellingPrice;


    //总盈/亏：1.0007200000000001
    String totalProfitAndLoss;

    //净盈/亏：1.0007200000000001
    String netProfitAndLoss;

    //是否是买入
    Boolean isBuy;
}
