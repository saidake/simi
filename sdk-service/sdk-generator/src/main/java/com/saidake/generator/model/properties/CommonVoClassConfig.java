package com.saidake.generator.model.properties;

import lombok.Data;

import java.util.List;

@Data
public class CommonVoClassConfig {
    private List<String> name;
    private List<ServiceParamsConfig> fieldList;
}
