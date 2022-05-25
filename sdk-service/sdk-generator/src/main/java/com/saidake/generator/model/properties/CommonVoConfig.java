package com.saidake.generator.model.properties;

import lombok.Data;

import java.util.List;

@Data
public class CommonVoConfig {
    private String folder;
    private List<CommonVoClassConfig> voList;

}
