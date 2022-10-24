package com.saidake.citi.domain.qihuo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
public class VarietyData extends BaseInfo {
    //========================================================== 基础数据
    @Schema(description = "名称")
    private String name;
}
