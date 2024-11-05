package com.simi.trade.domain.qihuo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
public class StrategyData extends TotalInfo {
    //========================================================== 基础数据
    @Schema(description = "名称")
    private String name;

    @Schema(description = "类型")
    private String rule;
}
