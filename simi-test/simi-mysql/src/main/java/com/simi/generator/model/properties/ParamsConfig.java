package com.simi.generator.model.properties;

import lombok.Data;

//计算的参数字段
@Data
public  class ParamsConfig{
    // 公共参数
    private String separator;
    private String srcPath;
    private String resourcePath;

    private String startUpperTemplateTableName;
    private String startLowerTemplateTableName;


    // 计算后的 包路径
    private String writeDtoRequestFolderPath;
    private String writeDtoResponseFolderPath;
    private String writeEntityFolderPath;
//    private String writeVoCommonFolderPath;

    private String writeServiceFolderPath;
    private String writeControllerFolderPath;
    private String writeMapperFolderPath;
    private String writeMapperResourcesFolderPath;


    // 读取的模板文件
    private String readTemplateDtoRequestFilePath;
    private String readTemplateDtoResponseFilePath;

    private String readTemplateDtoAddRequestFilePath;
    private String readTemplateDtoUpdateRequestFilePath;
    private String readTemplateDtoServiceFilePath;
//    private String readTemplateVoCommonFilePath;

    private String readTemplateEntityFilePath;
    private String readTemplateServiceFilePath;
    private String readTemplateServiceImplFilePath;
    private String readTemplateControllerFilePath;
    private String readTemplateMapperFilePath;
    private String readTemplateMapperResourcesFilePath;

}