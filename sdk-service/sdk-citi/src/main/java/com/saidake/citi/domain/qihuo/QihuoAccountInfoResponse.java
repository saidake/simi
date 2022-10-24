package com.saidake.citi.domain.qihuo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.HashMap;
import java.util.Map;

@Data
public class QihuoAccountInfoResponse  extends BaseInfo {
    //========================================================== 品种数据
    @Schema(description = "品种数据集合")
    private Map<String,VarietyData> varietyDataMap=new HashMap<>();

}
