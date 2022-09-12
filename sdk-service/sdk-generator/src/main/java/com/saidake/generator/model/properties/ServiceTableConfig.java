package com.saidake.generator.model.properties;


import lombok.Data;

import java.util.List;

@Data
public class ServiceTableConfig {
    private String table;
    private ServiceFieldsConfig addHandler;
    private ServiceFieldsConfig updateHandler;
    private List<ServiceMethodConfig> special;
}
