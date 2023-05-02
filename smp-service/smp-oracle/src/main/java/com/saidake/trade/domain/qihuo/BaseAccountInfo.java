package com.saidake.trade.domain.qihuo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.Map;

@Data
public class BaseAccountInfo  extends TotalInfo {
    //========================================================== 品种数据

    @Schema(description = "品种数据集合")
    private Map<String, VarietyData> varietyDataMap;

}
