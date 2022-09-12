package com.saidake.generator.model.properties;

import lombok.Data;

import java.util.List;

@Data
public class ServiceParamsConfig {
    private List<String> info;
    private List<String> check;
}
