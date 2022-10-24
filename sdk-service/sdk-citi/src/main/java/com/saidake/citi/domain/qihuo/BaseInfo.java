package com.saidake.citi.domain.qihuo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
@Schema(name = "base info")
public class BaseInfo {
    //========================================================== 前置数据
    @Schema(description = "总盈利次数")
    private Integer totalProfitTimes=0;

    @Schema(description = "总亏损次数")
    private Integer totalLossTimes=0;

    @Schema(description = "总交易次数")
    private Integer totalTradeTimes=0;

    //========================================================== 宏观数据
    @Schema(description = "总净盈利")
    private Integer totalNetProfit=0;

    @Schema(description = "总胜率")
    private String totalWiningRate;
}
