package com.saidake.generator.model.properties;


import lombok.Data;

@Data
public  class CoreConfig{
    private String dtoRequestPackage;
    private String dtoResponsePackage;

    private String entityPackage;

    private String servicePackage;
    private String controllerPackage;
    private String mapperPackage;
    private String mapperResourcesFolder;
}