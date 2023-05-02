package com.saidake.generator.model.properties;

import lombok.Data;

@Data
public  class SuffixConfig{
    private String serviceSuffix;
    private String serviceImplSuffix;
    private String controllerSuffix;
    private String mapperSuffix;
    private String mapperResourcesSuffix;

    private String entitySuffix;

    private String dtoRequestSuffix;
    private String dtoResponseSuffix;

    private String dtoAddRequestSuffix;
    private String dtoUpdateRequestSuffix;

    private String methodSuffix;
}