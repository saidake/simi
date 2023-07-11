package com.simi.generator.model.properties;


import lombok.Data;

import java.util.List;

@Data
public class ServiceMethodConfig {
    private String method;
    private List<String> name;
    private List<ServiceParamsConfig> request;
    private List<ServiceParamsConfig> response;

}
