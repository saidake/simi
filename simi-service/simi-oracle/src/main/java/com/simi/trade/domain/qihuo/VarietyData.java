package com.simi.trade.domain.qihuo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.HashMap;
import java.util.Map;

@Data
public class VarietyData extends TotalInfo {
    //========================================================== 基础数据
    @Schema(description = "名称")
    private String name;

    @Schema(description = "编码")
    private String code;

    @Schema(description = "品种策略数据")
    private Map<String, StrategyData> strategyDataMap=new HashMap<>();

}
