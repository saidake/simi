package com.saidake.generator.model.properties;

import lombok.Data;

import java.util.List;

@Data
public class AppendCommonVoConfig {
    private Boolean enabled;
    private CommonVoParamsConfig commonVoParamsConfig;
    private List<CommonVoConfig> voList;

}
