//package com.saidake.generator.AAAconfig.properties;
//
//
//import com.saidake.common.core.util.data.StringUtil;
//import com.saidake.common.core.util.file.FileUtil;
//import com.saidake.generator.model.properties.*;
//import com.saidake.generator.service.generator.impl.GeneratorTemplateImpl;
//import lombok.Data;
//import org.springframework.boot.context.properties.ConfigurationProperties;
//import org.springframework.stereotype.Component;
//import javax.annotation.PostConstruct;
//import java.util.List;
//
///**
// * 生成器核心配置项
// */
//@Data
//@Component
//@ConfigurationProperties(prefix = "sdk.generator")
//public class GeneratorProperties {
//    public static String javaSuffix=".java";
//    public static String xmlSuffix=".xml";
//    public static String serviceImplFolder="impl";
//
//    private String cachePath;
//    private String projectRootPath;
//    private TemplateConfig template;
//    private CoreConfig core;
//    private IOConfig io;
//    private SuffixConfig suffix;
//    private DbConfig db;
//    private ParamsConfig params;
//
//    private List<ServiceTableConfig> services;
//    private List<ServiceTableConfig> appendServices;
//
//
//    private List<AppendCommonVoConfig> appendCommonVo;
//
//
//
//    //初始化 公共参数
//    private void initCommonParams(ParamsConfig params) {
//        params.setSeparator( System.getProperty("line.separator"));
//        params.setSrcPath(this.getProjectRootPath()+"\\src\\main\\java");
//        params.setResourcePath(this.getProjectRootPath()+"\\src\\main\\resources");
//    }
//
//    //初始化 追加vo 参数
//    private void initAppendCommonVoParams(ParamsConfig params) {
////        params.setReadTemplateVoCommonFilePath(FileUtil.joinPathAndPackage(params.getSrcPath(),this.getAppendCommonVoParams().getTemplate())+GeneratorProperties.javaSuffix);
////        params.setWriteVoCommonFolderPath(FileUtil.joinPathAndPackage(params.getSrcPath(),this.getAppendCommonVoParams().getCommonVoPackage()));
//    }
//
//    @PostConstruct
//    public void initParams(){
//        //1. 初始化公共参数处理
//        ParamsConfig params = new ParamsConfig();
//        this.setParams(params);
//        initCommonParams(params);
//
//        String srcPath=params.getSrcPath();
//        String resourcePath=params.getResourcePath();
//
//        //2. 初始化 公共vo参数
//        initAppendCommonVoParams(params);
//
//        //2. 初始化写入 文件夹 的路径
//        params.setWriteDtoRequestFolderPath(FileUtil.joinPathAndPackage(srcPath,this.getCore().getDtoRequestPackage()));
//        params.setWriteDtoResponseFolderPath(FileUtil.joinPathAndPackage(srcPath,this.getCore().getDtoResponsePackage()));
//
//        params.setWriteEntityFolderPath(FileUtil.joinPathAndPackage(srcPath,this.getCore().getEntityPackage()));
//
//        params.setWriteServiceFolderPath(FileUtil.joinPathAndPackage(srcPath,this.getCore().getServicePackage()));
//        params.setWriteControllerFolderPath(FileUtil.joinPathAndPackage(srcPath,this.getCore().getControllerPackage()));
//        params.setWriteMapperFolderPath(FileUtil.joinPathAndPackage(srcPath,this.getCore().getMapperPackage()));
//        params.setWriteMapperResourcesFolderPath(FileUtil.joinPathAndPackage(resourcePath,this.getCore().getMapperResourcesFolder()));
//
//        params.setStartLowerTemplateTableName(StringUtil.startLower(StringUtil.lineToHump(this.getTemplate().getTableName())));
//        params.setStartUpperTemplateTableName(StringUtil.startUpper(StringUtil.lineToHump(this.getTemplate().getTableName())));
//
//        //3. 判断 是否有 单独的 表文件夹，设置读文件路径
//        String startUpperTemplateTableName = this.getParams().getStartUpperTemplateTableName();
//        String startLowerTemplateTableName = this.getParams().getStartLowerTemplateTableName();
//        Boolean hasServiceTableFolder = this.getIo().getHasServiceTableFolder();
//        Boolean hasControllerTableFolder = this.getIo().getHasControllerTableFolder();
//        Boolean hasMapperResourcesTableFolder = this.getIo().getHasMapperResourcesTableFolder();
//        Boolean hasDtoTableFolder = this.getIo().getHasDtoTableFolder();
//
//
//        //确定读取的 dto模板文件路径
//        params.setReadTemplateDtoRequestFilePath(GeneratorTemplateImpl.getTableWriteFilePath(startUpperTemplateTableName,startLowerTemplateTableName,params.getWriteDtoRequestFolderPath(),suffix.getDtoRequestSuffix(),
//                GeneratorProperties.javaSuffix,hasDtoTableFolder,false)
//        );
//        params.setReadTemplateDtoResponseFilePath(GeneratorTemplateImpl.getTableWriteFilePath(startUpperTemplateTableName,startLowerTemplateTableName,params.getWriteDtoRequestFolderPath(),suffix.getDtoResponseSuffix(),
//                GeneratorProperties.javaSuffix,hasDtoTableFolder,false)
//        );
//
//        params.setReadTemplateDtoAddRequestFilePath(GeneratorTemplateImpl.getTableWriteFilePath(startUpperTemplateTableName,startLowerTemplateTableName,params.getWriteDtoRequestFolderPath(),suffix.getDtoAddRequestSuffix(),
//                GeneratorProperties.javaSuffix,hasDtoTableFolder,false)
//        );
//        params.setReadTemplateDtoUpdateRequestFilePath(GeneratorTemplateImpl.getTableWriteFilePath(startUpperTemplateTableName,startLowerTemplateTableName,params.getWriteDtoRequestFolderPath(),suffix.getDtoUpdateRequestSuffix(),
//                GeneratorProperties.javaSuffix,hasDtoTableFolder,false)
//        );
//
//
//
//        //确定读取的 业务模板文件路径
//        params.setReadTemplateDtoServiceFilePath(GeneratorTemplateImpl.getTableWriteFilePath(startUpperTemplateTableName, startLowerTemplateTableName, params.getWriteDtoRequestFolderPath(), suffix.getDtoRequestSuffix(),
//                GeneratorProperties.javaSuffix, hasDtoTableFolder, false));
//
//        //确定读取的 模板文件路径
//        params.setReadTemplateServiceFilePath( GeneratorTemplateImpl.getTableWriteFilePath(startUpperTemplateTableName, startLowerTemplateTableName, params.getWriteServiceFolderPath(), suffix.getServiceSuffix(),
//                GeneratorProperties.javaSuffix, hasServiceTableFolder, false));
//        params.setReadTemplateServiceImplFilePath(GeneratorTemplateImpl.getTableWriteFilePath(startUpperTemplateTableName, startLowerTemplateTableName, params.getWriteServiceFolderPath(), suffix.getServiceImplSuffix(),
//                GeneratorProperties.javaSuffix, hasServiceTableFolder, true));
//        params.setReadTemplateMapperFilePath(GeneratorTemplateImpl.getTableWriteFilePath(startUpperTemplateTableName, startLowerTemplateTableName, params.getWriteMapperFolderPath(), suffix.getMapperSuffix(),
//                GeneratorProperties.javaSuffix, false, false));
//        params.setReadTemplateMapperResourcesFilePath(GeneratorTemplateImpl.getTableWriteFilePath(startUpperTemplateTableName, startLowerTemplateTableName, params.getWriteMapperResourcesFolderPath(), suffix.getMapperResourcesSuffix(),
//                GeneratorProperties.xmlSuffix, hasMapperResourcesTableFolder, false));
//        params.setReadTemplateControllerFilePath(GeneratorTemplateImpl.getTableWriteFilePath(startUpperTemplateTableName, startLowerTemplateTableName, params.getWriteControllerFolderPath(), suffix.getControllerSuffix(),
//                GeneratorProperties.javaSuffix, hasControllerTableFolder, false));
//        params.setReadTemplateEntityFilePath(GeneratorTemplateImpl.getTableWriteFilePath(startUpperTemplateTableName, startLowerTemplateTableName, params.getWriteEntityFolderPath(), suffix.getEntitySuffix(), GeneratorProperties.javaSuffix, false, false));
//
//    }
//}
