package com.simi.service.trade.domain.qihuo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.math.BigDecimal;

@Data
@Schema(name = "base info")
public class TotalInfo {
    //========================================================== 前置数据
    @Schema(description = "总交易次数")
    private Integer totalTradeTimes=0;

    @Schema(description = "总盈利次数")
    private Integer totalProfitTimes=0;

    @Schema(description = "总亏损次数")
    private Integer totalLossTimes=0;

    //========================================================== 宏观数据
    @Schema(description = "总净盈利")
    private BigDecimal totalNetProfit=new BigDecimal(0);

    @Schema(description = "总胜率")
    private BigDecimal totalWiningRate;

    @Schema(description = "基础本金")
    private Integer baseAssets;

    @Schema(description = "盈亏百分比")
    private BigDecimal winPercent=new BigDecimal(0);
}
