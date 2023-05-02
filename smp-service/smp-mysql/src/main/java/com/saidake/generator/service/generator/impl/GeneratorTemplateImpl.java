//package com.saidake.generator.service.generator.impl;
//
//import com.saidake.common.core.util.data.*;
//import com.saidake.generator.AAAconfig.properties.GeneratorProperties;
//import com.saidake.generator.model.properties.*;
//import com.saidake.generator.mybatis.entity.ColumnEntity;
//import com.saidake.generator.mybatis.entity.TableEntity;
//import com.saidake.generator.mybatis.mapper.DatabaseMapper;
//import com.saidake.generator.service.generator.GeneratorTemplate;
//import com.saidake.common.core.util.file.FileUtil;
//import lombok.extern.slf4j.Slf4j;
//import org.apache.commons.lang3.StringUtils;
//import org.springframework.stereotype.Service;
//
//import javax.annotation.Resource;
//import java.io.*;
//import java.util.*;
//import java.util.concurrent.atomic.AtomicBoolean;
//import java.util.concurrent.atomic.AtomicInteger;
//import java.util.stream.Collectors;
//import java.util.stream.Stream;
//
//
//@Service
//@Slf4j
//public class GeneratorTemplateImpl implements GeneratorTemplate {
//    @Resource
//    private GeneratorProperties generatorProperties;
//
//    @Resource
//    private DatabaseMapper databaseMapper;
//
//    @Override
//    public void clearOtherFiles() {
//        List<String> excludeFileList = new ArrayList<>();
//        ParamsConfig params = generatorProperties.getParams();
//        excludeFileList.add(params.getStartLowerTemplateTableName());
//        excludeFileList.add(params.getStartUpperTemplateTableName());
//        excludeFileList.add(GeneratorProperties.serviceImplFolder);
//        Stream.of(params.getWriteDtoRequestFolderPath(), params.getWriteDtoResponseFolderPath(),
//                params.getWriteServiceFolderPath(), params.getWriteControllerFolderPath(), params.getWriteMapperFolderPath(), params.getWriteMapperResourcesFolderPath(),
//                params.getWriteEntityFolderPath()
//        ).forEach(item -> FileUtil.clearOtherFilesStartsWithByList(item, excludeFileList));
//    }
//
//    @Override
//    public void generateInitServicesFiles() {
//        ParamsConfig params = generatorProperties.getParams();
//        SuffixConfig suffix = generatorProperties.getSuffix();
//
//        Boolean hasServiceTableFolder = generatorProperties.getIo().getHasServiceTableFolder();
//        Boolean hasControllerTableFolder = generatorProperties.getIo().getHasControllerTableFolder();
//        Boolean hasMapperResourcesTableFolder = generatorProperties.getIo().getHasMapperResourcesTableFolder();
//        Boolean hasDtoTableFolder = generatorProperties.getIo().getHasDtoTableFolder();
//
//        //读取模板service文件（fileList顺序不能修改）
//        List<File> fileList = Stream.of(
//                params.getReadTemplateServiceFilePath(), params.getReadTemplateServiceImplFilePath(), params.getReadTemplateMapperFilePath(), params.getReadTemplateMapperResourcesFilePath(), params.getReadTemplateControllerFilePath(),
//                params.getReadTemplateEntityFilePath(), params.getReadTemplateDtoAddRequestFilePath(), params.getReadTemplateDtoUpdateRequestFilePath(), params.getReadTemplateDtoServiceFilePath()
//        ).map(File::new).collect(Collectors.toList());
//        List<BufferedReader> bufferedReaderList = fileList.stream().map(this::createBufferedReader).collect(Collectors.toList());
//
//        //读取必须生成的表，过滤跳过的表
//        List<String> generateTableList = generatorProperties.getDb().getGenerateTables();
//        List<String> passTableList = generatorProperties.getDb().getPassTables();
//        List<TableEntity> allTableList = databaseMapper.queryTableList(null);
//        CheckUtil.checkTrue(generateTableList == null && passTableList == null, "generate-tables，pass-tables cannot be empty at the same time.");
//        List<TableEntity> tableEntityList = new ArrayList<>();
//        if (generateTableList != null) {
//            tableEntityList = allTableList.stream().filter(item -> generateTableList.contains(item.getTableName())).collect(Collectors.toList());
//        } else if (passTableList != null) {
//            tableEntityList = allTableList.stream().filter(item -> !passTableList.contains(item.getTableName())).collect(Collectors.toList());
//        }
//        //用读取到的表创建service文件
//        //START//==================================================================== foreach 写入模板文件
//        //获取读取文件大小
//        List<Integer> readFileAheadList = fileList.stream().map(item -> (int) item.length() + 1).collect(Collectors.toList());
//        tableEntityList.forEach(item -> {
//            //获取当前表名
//            String currentTableName = StringUtil.lineToHump(item.getTableName());
//            String currentStartUpperTableName = StringUtil.startUpper(currentTableName);
//            String currentStartLowerTableName = StringUtil.startLower(currentTableName);
//            //1. 写模板文件，获取 将要生成的文件路径【service，serviceImpl，mapper，mapperResources，com.saidake.citi.controller】
//            String currentTableServiceFilePath = getTableWriteFilePath(currentStartUpperTableName, currentStartLowerTableName, params.getWriteServiceFolderPath(), suffix.getServiceSuffix(),
//                    GeneratorProperties.javaSuffix, hasServiceTableFolder, false);
//            String currentTableServiceImplFilePath = getTableWriteFilePath(currentStartUpperTableName, currentStartLowerTableName, params.getWriteServiceFolderPath(), suffix.getServiceImplSuffix(), GeneratorProperties.javaSuffix, hasServiceTableFolder, true);
//            String currentTableMapperFilePath = getTableWriteFilePath(currentStartUpperTableName, currentStartLowerTableName, params.getWriteMapperFolderPath(), suffix.getMapperSuffix(), GeneratorProperties.javaSuffix, false, false);
//            String currentTableMapperResourcesFilePath = getTableWriteFilePath(currentStartUpperTableName, currentStartLowerTableName, params.getWriteMapperResourcesFolderPath(), suffix.getMapperResourcesSuffix(), GeneratorProperties.xmlSuffix, hasMapperResourcesTableFolder, false);
//            String currentTableControllerFilePath = getTableWriteFilePath(currentStartUpperTableName, currentStartLowerTableName, params.getWriteControllerFolderPath(), suffix.getControllerSuffix(), GeneratorProperties.javaSuffix, hasControllerTableFolder, false);
//            //此顺序必须和fileList顺序对应，filePathList和bufferedWriterList是没有entity的（排除了entity）
//            List<String> currentWriteFilePathList = Stream.of(currentTableServiceFilePath, currentTableServiceImplFilePath, currentTableMapperFilePath, currentTableMapperResourcesFilePath, currentTableControllerFilePath).collect(Collectors.toList());
//            List<BufferedWriter> bufferedWriterList = currentWriteFilePathList.stream().map(item33 -> createBufferedWriter(item33, false)).collect(Collectors.toList());
//            // service  serviceImpl   mapper  mapperResources  com.saidake.citi.controller
//            HashMap<Integer, String> checkMap = MapUtil.of(0, "service", 1, "serviceImpl", 2, "mapper", 3, "mapperResources", 4, "com.saidake.citi.controller");
//            List<ServiceTableConfig> services = generatorProperties.getServices();
//            ServiceTableConfig currentServiceTableConfig = services.stream().filter(item22 -> item22.getTable().equals(item.getTableName())).findFirst().orElse(null);
//            for (int i = 0; i < bufferedWriterList.size(); i++) {
//                writeTemplateFile(checkMap.get(i), bufferedReaderList.get(i), readFileAheadList.get(i), bufferedWriterList.get(i), currentWriteFilePathList.get(i),
////                        item.getTableName(), currentTableName, currentStartUpperTableName, currentStartLowerTableName, item.getTableComment());
//                        item, currentServiceTableConfig);
//            }
//
//            //2. 写entity文件
//            String currentTableEntityFilePath = getTableWriteFilePath(currentStartUpperTableName, currentStartLowerTableName, params.getWriteEntityFolderPath(), suffix.getEntitySuffix(), GeneratorProperties.javaSuffix, false, false);
//            BufferedWriter bufferedWriterEntity = createBufferedWriter(currentTableEntityFilePath, false);
////            writeTemplateFileCheckInside("entity", bufferedReaderList.get(5), readFileAheadList.get(5), bufferedWriterEntity,
////                    currentTableEntityFilePath, item.getTableName(), currentTableName, currentStartUpperTableName, currentStartLowerTableName, item.getTableComment(),currentServiceTableConfig);
//            writeTemplateFileCheckInside("entity", bufferedReaderList.get(5), readFileAheadList.get(5), bufferedWriterEntity, currentTableEntityFilePath,
//                    item, currentServiceTableConfig, null);
//
//            //3. 写dto addHandler文件
//            String currentTableDtoAddRequestFilePath = getTableWriteFilePath(currentStartUpperTableName, currentStartLowerTableName, params.getWriteDtoRequestFolderPath(), suffix.getDtoAddRequestSuffix(), GeneratorProperties.javaSuffix, hasDtoTableFolder, false);
//            BufferedWriter bufferedWriterDtoAddRequest = createBufferedWriter(currentTableDtoAddRequestFilePath, false);
//            writeTemplateFileCheckInside("dtoAddRequest", bufferedReaderList.get(6), readFileAheadList.get(6), bufferedWriterDtoAddRequest, currentTableDtoAddRequestFilePath,
//                    item, currentServiceTableConfig, null);
//            //4. 写dto updateRequest文件
//            String currentTableDtoUpdateRequestFilePath = getTableWriteFilePath(currentStartUpperTableName, currentStartLowerTableName, params.getWriteDtoRequestFolderPath(), suffix.getDtoUpdateRequestSuffix(), GeneratorProperties.javaSuffix, hasDtoTableFolder, false);
//            BufferedWriter bufferedWriterUpdateAddRequest = createBufferedWriter(currentTableDtoUpdateRequestFilePath, false);
//            writeTemplateFileCheckInside("dtoUpdateRequest", bufferedReaderList.get(7), readFileAheadList.get(7), bufferedWriterUpdateAddRequest, currentTableDtoUpdateRequestFilePath,
//                    item, currentServiceTableConfig, null);
//            //4. 写dto specialRequest文件（模板：getReadTemplateDtoServiceFilePath）
//            if (currentServiceTableConfig != null && currentServiceTableConfig.getSpecial() != null) {
//                currentServiceTableConfig.getSpecial().forEach(itemSpecial -> {
//                    String name = itemSpecial.getName().get(0);
//                    String startUpper = StringUtil.startUpper(name);
//                    String currentTableDtoRequestFilePath = getTableWriteFilePath(StringUtils.join(currentStartUpperTableName, startUpper), currentStartLowerTableName, params.getWriteDtoRequestFolderPath(), suffix.getDtoRequestSuffix(), GeneratorProperties.javaSuffix, hasDtoTableFolder, false);
//                    BufferedWriter bufferedWriterDtoRequest = createBufferedWriter(currentTableDtoRequestFilePath, false);
//                    writeTemplateFileCheckInside("special", bufferedReaderList.get(8), readFileAheadList.get(8), bufferedWriterDtoRequest, currentTableDtoRequestFilePath,
//                            item, currentServiceTableConfig, itemSpecial);
//                });
//            }
//        });
//        bufferedReaderList.forEach(this::closeBufferedReader);
//        //END  //==================================================================== foreach 写入模板文件
//    }
//
//    //写入模板文件   0 service 1 serviceImpl  2  com.saidake.citi.controller
//    @Override
//    public void writeTemplateFileAppend(String writeType, List<String> bufferedReaderString,
//                                        BufferedWriter bufferedWriter,
//                                        String currentWriteFilePath,
//                                        ServiceTableConfig currentServiceTableConfig
//    ) {
//        // 1. 获取表名 信息
//        String currentTableName = StringUtil.lineToHump(currentServiceTableConfig.getTable());
//        String currentStartUpperTableName = StringUtil.startUpper(currentTableName);
//        String currentStartLowerTableName = StringUtil.startLower(currentTableName);
//        String separator = System.getProperty("line.separator");
//        // 2. 遍历前的准备工作
//        AtomicInteger enterTime = new AtomicInteger();
//        AtomicBoolean hasEnter = new AtomicBoolean(false);
//        // 3. 遍历 每一行
//        try {
//            for (String readLineString : bufferedReaderString) {
//                // 1). 判断 当前行在哪个位置
//                Integer count1 = StringUtil.countSubString(readLineString, "{");
//                Integer count2 = StringUtil.countSubString(readLineString, "}");
//                if (count1 > 0) {
//                    enterTime.addAndGet(count1);
//                    hasEnter.set(true);
//                }
//                if (count2 > 0) {
//                    enterTime.addAndGet(-count2);
//                }
//                // 2). 如果在 第一行的package
//                if (readLineString.startsWith("package ")) {
//                    StringBuilder readServiceLineBuilderStartPackage = new StringBuilder(readLineString);
//                    readServiceLineBuilderStartPackage.append(separator);
//                    // 拿到special，全都import 特殊服务的 LoginDtoRequest
//                    writeImportSet(readServiceLineBuilderStartPackage,importPostTypeDefinition(currentServiceTableConfig));
//                    bufferedWriter.write(readServiceLineBuilderStartPackage.toString());
//                }
//                // 3). 在末尾，包含特殊服务时，com.saidake.citi.controller 写特殊服务
//                else if (hasEnter.get() && enterTime.get() == 0) {
//                    StringBuilder readServiceLineBuilderEnd = new StringBuilder(separator);
//                    List<ServiceMethodConfig> serviceMethodConfigList = currentServiceTableConfig.getSpecial();
//                    serviceMethodConfigList.forEach(item -> writeMethod(writeType, currentStartUpperTableName, currentStartLowerTableName, readServiceLineBuilderEnd, item));
//                    readServiceLineBuilderEnd.append(separator);
//                    readServiceLineBuilderEnd.append("}");
//                    bufferedWriter.write(readServiceLineBuilderEnd.toString());
//                    break;
//                } else {
//                    bufferedWriter.write(readLineString);  //其他情况直接写
//                    bufferedWriter.write(separator);
//                }
//            }
//            bufferedWriter.close();
//        } catch (IOException e) {
//            e.printStackTrace();
//            log.info("写入{}文件失败: {}", writeType, currentWriteFilePath);
//        }
//        log.info("写入{}文件成功: {}", writeType, currentWriteFilePath);
//    }
//    @Override
//    public void   writeImportSet(StringBuilder stringBuilder, Set<String> stringSet){
//        String separator = System.getProperty("line.separator");
//        stringBuilder.append(separator);
//        stringBuilder.append(StringUtils.join(stringSet,separator));
//        stringBuilder.append(separator);
//    }
//
//    @Override
//    public Set<String>  importPostTypeDefinition(ServiceTableConfig serviceTableConfig) {
////        String dtoRequestPackage, Boolean hasDtoTableFolder, String dtoRequestSuffix, String currentStartUpperTableName, String currentStartLowerTableName, String separator,
//        List<ServiceMethodConfig> special = serviceTableConfig.getSpecial();
//        String tableName = serviceTableConfig.getTable();
//        String lineToHump = StringUtil.lineToHump(tableName);
//        String currentStartUpperTableName=StringUtil.startUpper(lineToHump);
//        String currentStartLowerTableName=StringUtil.startLower(lineToHump);
//        Boolean hasDtoTableFolder=generatorProperties.getIo().getHasDtoTableFolder();
//        String dtoRequestPackage=generatorProperties.getCore().getDtoRequestPackage();
//        Set<String> stringSet = new HashSet<>();
//        for (ServiceMethodConfig item : special) {
//            // get就只导入 JavaType的类型定义
//            if("get".equals(item.getMethod())){
//                stringSet.addAll(importTypeDefinitionByJavaType(item.getRequest(),false));
//            // post就只导入 dto的定义
//            }else{
//                String currentLoginName = item.getName().get(0);
//                String currentPackage;
//                String currentLoginRequestName = currentStartUpperTableName + StringUtil.startUpper(currentLoginName) + generatorProperties.getSuffix().getDtoRequestSuffix();
//                if (hasDtoTableFolder) {
//                    currentPackage = dtoRequestPackage + "." + currentStartLowerTableName;
//                } else {
//                    currentPackage = dtoRequestPackage;
//                }
//                stringSet.add(String.format("import %s.%s;", currentPackage, currentLoginRequestName));
//            }
//        }
//        return stringSet;
//    }
//
//
//    @Override
//    public void generateAppendServices() {
//        List<ServiceTableConfig> appendServices = generatorProperties.getAppendServices();
//        ParamsConfig params = generatorProperties.getParams();
//        SuffixConfig suffix = generatorProperties.getSuffix();
//        Boolean hasDtoTableFolder = generatorProperties.getIo().getHasDtoTableFolder();
//
//        //START//==================================================================== 遍历 appendServices
//        // 1. 直接遍历 追加的services
//        appendServices.forEach(serviceTableConfigItem -> {
//
//            // 1). 获取当前 表名 信息
//            String currentTableName = StringUtil.lineToHump(serviceTableConfigItem.getTable());
//            String currentStartUpperTableName = StringUtil.startUpper(currentTableName);
//            String currentStartLowerTableName = StringUtil.startLower(currentTableName);
//
//            // 2). 获取Controller，Service，ServiceImpl文件路径
//            String currentTableServiceFilePath = getTableWriteFilePath(currentStartUpperTableName, currentStartLowerTableName, params.getWriteServiceFolderPath(), suffix.getServiceSuffix(),
//                    GeneratorProperties.javaSuffix, generatorProperties.getIo().getHasServiceTableFolder(), false);
//            String currentTableServiceImplFilePath = getTableWriteFilePath(currentStartUpperTableName, currentStartLowerTableName, params.getWriteServiceFolderPath(), suffix.getServiceImplSuffix(),
//                    GeneratorProperties.javaSuffix, generatorProperties.getIo().getHasServiceTableFolder(), true);
//            String currentTableControllerFilePath = getTableWriteFilePath(currentStartUpperTableName, currentStartLowerTableName, params.getWriteControllerFolderPath(), suffix.getControllerSuffix(),
//                    GeneratorProperties.javaSuffix, generatorProperties.getIo().getHasControllerTableFolder(), false);
//
//            // 3). 存储Controller，Service，ServiceImpl文件 到内存 storageReadLineList
//            HashMap<Integer, String> checkMap = MapUtil.of(0, "service", 1, "serviceImpl", 2, "com.saidake.citi.controller");  // 各个索引都是什么东西
//            List<File> fileList = Stream.of(currentTableServiceFilePath, currentTableServiceImplFilePath, currentTableControllerFilePath).map(File::new).collect(Collectors.toList());
//            List<List<String>> storageReadLineList = fileList.stream().map(itemFile -> {
//                BufferedReader currentBufferedReader = createBufferedReader(itemFile);
//                List<String> currentStringStorageList = new ArrayList<>();
//                try {
//                    String readLine = currentBufferedReader.readLine();
//                    while (readLine != null) {
//                        currentStringStorageList.add(readLine);
//                        readLine = currentBufferedReader.readLine();
//                    }
//                    // 随手关门
//                    currentBufferedReader.close();
//                } catch (IOException e) {
//                    e.printStackTrace();
//                }
//                return currentStringStorageList;
//            }).collect(Collectors.toList());
//            // 4). 根据storageReadLineList，写Controller，Service，ServiceImpl文件，并添加额外的 services
//            List<String> currentWriteFilePathList = Stream.of(currentTableServiceFilePath, currentTableServiceImplFilePath, currentTableControllerFilePath).collect(Collectors.toList());
//            List<BufferedWriter> bufferedWriterList = currentWriteFilePathList.stream().map(item33 -> createBufferedWriter(item33, false)).collect(Collectors.toList());  // 直接覆盖，注意此操作
//            for (int i = 0; i < bufferedWriterList.size(); i++) {
//                writeTemplateFileAppend(checkMap.get(i), storageReadLineList.get(i), bufferedWriterList.get(i), currentWriteFilePathList.get(i), serviceTableConfigItem);
//            }
//            // 5). 读 dto request文件
//            File dtoRequestFileTemplateFile = new File(params.getReadTemplateDtoRequestFilePath());
//            int readAheadRequestFile = (int) dtoRequestFileTemplateFile.length() + 1;
//            BufferedReader bufferedReaderDtoRequest = createBufferedReader(dtoRequestFileTemplateFile);
//            // 6). 写dto specialRequest文件（模板：getReadTemplateDtoServiceFilePath）
//            serviceTableConfigItem.getSpecial().forEach(itemMethodConfigSpecial -> {
//                String name = itemMethodConfigSpecial.getName().get(0);
//                String startUpper = StringUtil.startUpper(name);
//
//                // 写 dto request文件
//                String currentLoginDtoRequestFilePath = getTableWriteFilePath(StringUtils.join(currentStartUpperTableName, startUpper), currentStartLowerTableName, params.getWriteDtoRequestFolderPath(), suffix.getDtoRequestSuffix(),
//                        GeneratorProperties.javaSuffix, hasDtoTableFolder, false);
//                BufferedWriter bufferedWriterLoginDtoRequest = createBufferedWriter(currentLoginDtoRequestFilePath, false);
//                writeSpecialDto("request", bufferedReaderDtoRequest, readAheadRequestFile, bufferedWriterLoginDtoRequest, currentLoginDtoRequestFilePath,
//                        serviceTableConfigItem, itemMethodConfigSpecial);
//            });
//        });
//        //END  //==================================================================== 遍历 appendServices
//    }
//
//
//    //写入dto request文件
//    @Override
//    public void writeSpecialDto(
//            String writeType,
//            BufferedReader bufferedReader,
//            int readEntityFileAheadLimit,
//            BufferedWriter bufferedWriter,
//            String currentTableServiceFilePath,
//
//            ServiceTableConfig currentServiceTableConfig,
//            ServiceMethodConfig currentServiceMethodConfig
//    ) {
//        // 1. 获取表名 信息，和公共数据
//        ParamsConfig params = generatorProperties.getParams();
//        String currentTableName = StringUtil.lineToHump(currentServiceTableConfig.getTable());
//        String currentStartUpperTableName = StringUtil.startUpper(currentTableName);
//        String currentStartLowerTableName = StringUtil.startLower(currentTableName);
//        List<ServiceParamsConfig> LoginRequestList = currentServiceMethodConfig.getRequest();
//
//        try {
//            // 2. 先读一行
//            bufferedReader.mark(readEntityFileAheadLimit);
//            String readServiceLine = bufferedReader.readLine();
//            String separator = System.getProperty("line.separator");
//
//            // 3. 开始遍历
//            int enterTime = 0;
//            boolean hasEnter = false;
//            boolean isStart = true;
//            //START//========================================================================= 遍历每一行
//            while (readServiceLine != null) {
//                Integer count1 = StringUtil.countSubString(readServiceLine, "{");
//                Integer count2 = StringUtil.countSubString(readServiceLine, "}");
//                if (count1 > 0) {
//                    enterTime += count1;
//                    hasEnter = true;
//                }
//                if (count2 > 0) {
//                    enterTime -= count2;
//                }
//                // 1). 在首行       【写入拼接的builder】
//                if (readServiceLine.startsWith("package") && !hasEnter) {
//                    readServiceLine = readServiceLine.replace(params.getStartLowerTemplateTableName(), currentStartLowerTableName);  //小写表名替换，表示包名
//                    StringBuilder readServiceLineBuilderStart = new StringBuilder(readServiceLine);
//                    readServiceLineBuilderStart.append(separator);
//                    readServiceLineBuilderStart.append(separator);
//                    writeImportSet(readServiceLineBuilderStart,importTypeDefinitionByJavaType(LoginRequestList, true));
//                    bufferedWriter.write(readServiceLineBuilderStart.toString());
//                    // 2). 首次遇到 {   【写入拼接的builder】
//                } else if (hasEnter && isStart) {
//                    //依然需要对当前行进行 字符串替换
//                    String loginName = currentServiceMethodConfig.getName().get(0);
//                    readServiceLine = readServiceLine.replace(params.getStartUpperTemplateTableName(), currentStartUpperTableName + StringUtil.startUpper(loginName));  //大写表名替换
//
//                    StringBuilder readServiceLineBuilderReplace = new StringBuilder(readServiceLine);
//                    readServiceLineBuilderReplace.append(separator);
//                    readServiceLineBuilderReplace.append(separator);
//                    writeJavaVariableByJavaType(readServiceLineBuilderReplace, separator, LoginRequestList);
//                    isStart = false;
//                    bufferedWriter.write(readServiceLineBuilderReplace.toString());
//                    // 3). 不在内部{}       【直接写入readServiceLine】
//                } else if (enterTime == 0) {
//                    //对当前行进行 字符串替换
//                    String loginComment = currentServiceMethodConfig.getName().get(1);
//                    readServiceLine = readServiceLine.replace(generatorProperties.getTemplate().getTableComment(), loginComment);  //表注释替换【com.saidake.citi.controller】
//                    bufferedWriter.write(readServiceLine);
//                    bufferedWriter.write(separator);
//                }
//                // 4). 在内部直接跳过
//                readServiceLine = bufferedReader.readLine();
//            }
//            //END  //========================================================================= 遍历每一行
//            bufferedReader.reset();
//            bufferedWriter.close();
//        } catch (IOException ex) {
//            ex.printStackTrace();
//            log.info("写入特殊服务dto文件失败: {}", currentTableServiceFilePath);
//            return;
//        }
//        log.info("写入特殊服务dto文件成功: {}", currentTableServiceFilePath);
//    }
//
//    @Override
//    public void generateCommonVoClass() {
//        List<AppendCommonVoConfig> appendCommonVo = generatorProperties.getAppendCommonVo();
//        appendCommonVo.stream().filter(AppendCommonVoConfig::getEnabled).forEach(this::writeCommonVoClass);
//    }
//    @Override
//    public void writeCommonVoClass(AppendCommonVoConfig appendCommonVoConfig){
//        ParamsConfig params = generatorProperties.getParams();
//        CommonVoParamsConfig commonVoParamsConfig = appendCommonVoConfig.getCommonVoParamsConfig();
//        List<CommonVoConfig> appendCommonVoList = appendCommonVoConfig.getVoList();
//        //1. 获取读文件
//        String readTemplateVoCommonFilePath= FileUtil.joinPathAndPackage(params.getSrcPath(),commonVoParamsConfig.getTemplate())+GeneratorProperties.javaSuffix;
//        String writeVoCommonFolderPath= FileUtil.joinPathAndPackage(params.getSrcPath(),commonVoParamsConfig.getCommonVoPackage());
//
//        File voCommonFile = new File(readTemplateVoCommonFilePath);
//        int readAheadLimit = (int) voCommonFile.length() + 1;
//        BufferedReader bufferedReader = createBufferedReader(voCommonFile);
//        //1. 遍历配置，创建并写入新文件
//        appendCommonVoList.forEach(folderItem -> {
//            folderItem.getVoList().forEach(voItem -> {
//                String voItemName = voItem.getName().get(0);
//                String voItemComment = voItem.getName().get(1);
//                List<ServiceParamsConfig> fieldList = voItem.getFieldList();
//                String voItemPath = FileUtil.joinPath(writeVoCommonFolderPath, folderItem.getFolder(), StringUtils.join(voItemName, commonVoParamsConfig.getSuffix(), GeneratorProperties.javaSuffix));
//                BufferedWriter bufferedWriterVo = createBufferedWriter(voItemPath, !"overwrite".equals(commonVoParamsConfig.getWriteMode()));
//                writeTemplateVoFile(commonVoParamsConfig,bufferedReader, readAheadLimit, bufferedWriterVo, fieldList, voItemName, voItemComment, folderItem.getFolder());
//            });
//        });
//        try {
//            bufferedReader.close();
//        } catch (IOException e) {
//            e.printStackTrace();
//        }
//    }
//
//    //-------------------------------------------------------------------------------------- 业务辅助方法 -----------------------------------------------------------------------------//
//
//    @Override
//    public void writeTemplateVoFile(
//            CommonVoParamsConfig commonVoParamsConfig,
//            BufferedReader bufferedReader, int readAheadLimit, BufferedWriter bufferedWriter,
//            List<ServiceParamsConfig> fieldList, String voItemName, String voItemComment, String folder
//            ) {
//        ParamsConfig params = generatorProperties.getParams();
//        String separator = params.getSeparator();
//        try {
//            bufferedReader.mark(readAheadLimit);
//            int enterTime = 0;
//            boolean hasEnter = false;
//            boolean isStart = true;
//            String readServiceLine = bufferedReader.readLine();
//            while (readServiceLine != null) {
//                Integer count1 = StringUtil.countSubString(readServiceLine, "{");
//                Integer count2 = StringUtil.countSubString(readServiceLine, "}");
//                if (count1 > 0) {
//                    enterTime += count1;
//                    hasEnter = true;
//                }
//                if (count2 > 0) {
//                    enterTime -= count2;
//                }
//                //在package 一行   【写入拼接的builder】
//                if (readServiceLine.startsWith("package ") && !hasEnter) {
//                    //重写package
//                    readServiceLine="package "+commonVoParamsConfig.getCommonVoPackage()+"."+folder+";";
//                    StringBuilder readServiceLineBuilderStart = new StringBuilder(readServiceLine);
//                    readServiceLineBuilderStart.append(separator);
//                    readServiceLineBuilderStart.append(separator);
//                    writeImportSet(readServiceLineBuilderStart,importTypeDefinitionByJavaType(fieldList, true));
//                    bufferedWriter.write(readServiceLineBuilderStart.toString());
//                }
//                //首次遇到 {   【写入拼接的builder】
//                else if (hasEnter && isStart) {
//                    //依然需要对当前行进行 字符串替换
//                    readServiceLine = readServiceLine.replace(commonVoParamsConfig.getReplaceName(), voItemName);          //vo类名替换
//                    StringBuilder readServiceLineBuilderReplace = new StringBuilder(readServiceLine);
//                    readServiceLineBuilderReplace.append(separator);
//                    readServiceLineBuilderReplace.append(separator);
//                    writeJavaVariableByJavaType(readServiceLineBuilderReplace, separator, fieldList);
//                    bufferedWriter.write(readServiceLineBuilderReplace.toString());
//                    isStart = false;
//                }
//                //不在内部，直接替换   【写入拼接的builder】
//                else if (enterTime == 0) {
//                    //对当前行进行 字符串替换
//                    readServiceLine = readServiceLine.replace(commonVoParamsConfig.getReplaceComment(), voItemComment);        //vo注释替换
//                    bufferedWriter.write(readServiceLine);  //大写表名替换
//                    bufferedWriter.write(separator);
//                }
//                //在内部直接跳过
//                readServiceLine = bufferedReader.readLine();
//            }
//            //重置reader，关闭writer
//            bufferedReader.reset();
//            bufferedWriter.close();
//        } catch (IOException e) {
//            e.printStackTrace();
//        }
//
//    }
//
//
//    //写入entity文件
//    @Override
//    public void writeTemplateFileCheckInside(
//            String writeType,
//            BufferedReader bufferedReader,
//            int readEntityFileAheadLimit,
//            BufferedWriter bufferedWriter,
//            String currentTableServiceFilePath,
//
//            TableEntity tableEntity,
//            ServiceTableConfig currentServiceTableConfig,
//            ServiceMethodConfig currentServiceMethodConfig
//    ) {
//        // 1. 获取表名 信息，和公共数据
//        ParamsConfig params = generatorProperties.getParams();
//        String currentLineTableName = tableEntity.getTableName();
//        String currentTableComment = tableEntity.getTableComment();
//        String currentTableName = StringUtil.lineToHump(tableEntity.getTableName());
//        String currentStartUpperTableName = StringUtil.startUpper(currentTableName);
//        String currentStartLowerTableName = StringUtil.startLower(currentTableName);
//
//        try {
//            // 2. 先读一行
//            bufferedReader.mark(readEntityFileAheadLimit);
//            String readServiceLine = bufferedReader.readLine();
//            String separator = System.getProperty("line.separator");
//
//            // 3.1. 当写入类型是dtoAddRequest时：过滤 跳过的字段
//            List<String> passFields = new ArrayList<>(generatorProperties.getDb().getPassFields());
//            if ("dtoAddRequest".equals(writeType) && currentServiceTableConfig != null && currentServiceTableConfig.getAddHandler() != null) {
//                passFields.addAll(currentServiceTableConfig.getAddHandler().getRequestPassFields());
//            } else if ("dtoAddRequest".equals(writeType)) {
//                List<String> requestPassFields = generatorProperties.getDb().getRequestPassFields();
//                if (requestPassFields != null) passFields.addAll(requestPassFields);
//            }
//            List<ColumnEntity> columnEntityList;
//            List<String> distinctTableDataTypeList;
//            // 3.2. 当写入类型是special时：不写 数据库字段了
//            boolean isSpecial = "special".equals(writeType);
//            if (isSpecial) {
//                columnEntityList = new ArrayList<>();
//                distinctTableDataTypeList = currentServiceMethodConfig.getRequest().stream().map(item -> item.getInfo().get(2)).distinct().collect(Collectors.toList());
//                // 3.3. 当写入类型是dtoUpdateRequest时：只要id，其他都不要
//            } else if ("dtoUpdateRequest".equals(writeType)) {
//                columnEntityList = databaseMapper.queryColumnList(currentLineTableName, null);
//                ColumnEntity columnEntity = columnEntityList.stream().filter(item -> "id".equals(item.getColumnName())).findFirst().orElse(null);
//                if (columnEntity == null) {
//                    columnEntityList = new ArrayList<>();
//                    distinctTableDataTypeList = new ArrayList<>();
//                } else {
//                    columnEntityList = Arrays.asList(columnEntity);
//                    distinctTableDataTypeList = Collections.singletonList(columnEntity.getDataType());
//                }
//                // 3.4. 当写入类型是entity时：读数据库
//            } else {
//                columnEntityList = databaseMapper.queryColumnList(currentLineTableName, passFields);
//                distinctTableDataTypeList = databaseMapper.queryDistinctTableDataType(currentLineTableName, passFields);
//            }
//
//            // 4. 开始遍历
//            int enterTime = 0;
//            boolean hasEnter = false;
//            boolean isStart = true;
//            //START//========================================================================= 遍历每一行
//            while (readServiceLine != null) {
//                Integer count1 = StringUtil.countSubString(readServiceLine, "{");
//                Integer count2 = StringUtil.countSubString(readServiceLine, "}");
//                if (count1 > 0) {
//                    enterTime += count1;
//                    hasEnter = true;
//                }
//                if (count2 > 0) {
//                    enterTime -= count2;
//                }
//                // 1). 在首行       【写入拼接的builder】
//                if (readServiceLine.startsWith("package") && !hasEnter) {
//                    readServiceLine = readServiceLine.replace(params.getStartLowerTemplateTableName(), currentStartLowerTableName);  //小写表名替换，表示包名
//                    StringBuilder readServiceLineBuilderStart = new StringBuilder(readServiceLine);
//                    readServiceLineBuilderStart.append(separator);
//                    readServiceLineBuilderStart.append(separator);
//                    if (isSpecial) {
//                        List<ServiceParamsConfig> LoginRequestList = currentServiceMethodConfig.getRequest();
//                        writeImportSet(readServiceLineBuilderStart,importTypeDefinitionByJavaType(LoginRequestList, true));
//                    } else {
//                        importTypeDefinitionByDbType(readServiceLineBuilderStart, distinctTableDataTypeList, columnEntityList);
//                    }
//                    bufferedWriter.write(readServiceLineBuilderStart.toString());
//                    // 2). 首次遇到 {   【写入拼接的builder】
//                } else if (hasEnter && isStart) {
//                    //依然需要对当前行进行 字符串替换
//                    if (isSpecial) {
//                        String loginName = currentServiceMethodConfig.getName().get(0);
//                        readServiceLine = readServiceLine.replace(params.getStartUpperTemplateTableName(), currentStartUpperTableName + StringUtil.startUpper(loginName));  //大写表名替换
//                    } else {
//                        readServiceLine = readServiceLine.replace(params.getStartUpperTemplateTableName(), currentStartUpperTableName);  //大写表名替换
//                    }
//                    StringBuilder readServiceLineBuilderReplace = new StringBuilder(readServiceLine);
//                    readServiceLineBuilderReplace.append(separator);
//                    readServiceLineBuilderReplace.append(separator);
//                    if (isSpecial) {
//                        List<ServiceParamsConfig> LoginRequestList = currentServiceMethodConfig.getRequest();
//                        writeJavaVariableByJavaType(readServiceLineBuilderReplace, separator, LoginRequestList);
//                    } else {
//                        writeJavaVariableByDbType(readServiceLineBuilderReplace, separator, columnEntityList);
//                    }
//                    isStart = false;
//                    bufferedWriter.write(readServiceLineBuilderReplace.toString());
//                    // 3). 不在内部{}       【直接写入readServiceLine】
//                } else if (enterTime == 0) {
//                    //对当前行进行 字符串替换
//                    readServiceLine = readServiceLine.replace(params.getStartUpperTemplateTableName(), currentStartUpperTableName);  //大写表名替换
//                    readServiceLine = readServiceLine.replace(generatorProperties.getTemplate().getTableName(), currentLineTableName);    //下划线表名替换【mapper resources】
//                    if (isSpecial) {
//                        String loginComment = currentServiceMethodConfig.getName().get(1);
//                        readServiceLine = readServiceLine.replace(generatorProperties.getTemplate().getTableComment(), currentTableComment + loginComment);  //表注释替换【com.saidake.citi.controller】
//                        if (readServiceLine.contains("接口"))
//                            readServiceLine = readServiceLine.replace("表", "");  //表注释替换【com.saidake.citi.controller】
//                    } else {
//                        readServiceLine = readServiceLine.replace(generatorProperties.getTemplate().getTableComment(), currentTableComment);  //表注释替换【com.saidake.citi.controller】
//                    }
//                    bufferedWriter.write(readServiceLine);  //大写表名替换
//                    bufferedWriter.write(separator);
//                }
//                // 4). 在内部直接跳过
//                readServiceLine = bufferedReader.readLine();
//            }
//            //END  //========================================================================= 遍历每一行
//            bufferedReader.reset();
//            bufferedWriter.close();
//        } catch (IOException ex) {
//            log.info("write entity file failed: {}", currentTableServiceFilePath);
//            return;
//        }
//        log.info("write entity file successfully: {}", currentTableServiceFilePath);
//    }
//
//
//
//
//    /**
//     * import 类型，通过javaType
//     * @param loginRequestList       参数数组
//     */
//    @Override
//    public Set<String>  importTypeDefinitionByJavaType(List<ServiceParamsConfig> loginRequestList, Boolean needNotNull) {
//        Set<String> resultSet=new HashSet<>();
//        String separator = System.getProperty("line.separator");
//        //去重后的类型数组
//        Set<String> distinctJavaTypeList = loginRequestList.stream().map(loginFieldItem -> loginFieldItem.getInfo().get(2)).collect(Collectors.toSet());
//        //有没有nullable=NO
//        ServiceParamsConfig serviceParamsConfig = loginRequestList.stream().filter(item -> "required".equals(item.getInfo().get(3))).findFirst().orElse(null);
//        if (serviceParamsConfig != null && needNotNull) {
//            resultSet.add("import javax.validation.constraints.NotNull;");
//        }
//        //数据类型判断
//        if (distinctJavaTypeList.contains("BigDecimal")) {
//           resultSet.add("import java.math.BigDecimal;");
//        }
//        if (distinctJavaTypeList.contains("Date") || distinctJavaTypeList.contains("Timestamp")) {
//           resultSet.add("import java.util.Date;");
//           resultSet.add("import com.fasterxml.jackson.annotation.JsonFormat;");
//        } else if (distinctJavaTypeList.contains("Time")) {
//           resultSet.add("import java.sql.Time;");
//           resultSet.add("import com.fasterxml.jackson.annotation.JsonFormat;");
//        }
//        return resultSet;
//    }
//
//    @Override
//    public void writeJavaVariableByJavaType(StringBuilder readServiceLineBuilder, String separator, List<ServiceParamsConfig> loginRequestList) {
//        loginRequestList.forEach(item -> {
//            String columnName = item.getInfo().get(0);
//            String columnComment = item.getInfo().get(1);
//            boolean required = "required".equals(item.getInfo().get(3));
//            String startLower = StringUtil.startLower(StringUtil.lineToHump(columnName));
//            StringBuilder apiModel = new StringBuilder();
//            List<String> apiModelParamsList = new ArrayList<>();
//
//            //1. 写入@ApiModelProperty
//            apiModel.append("\t@ApiModelProperty(");
//            if (!StringUtils.isEmpty(columnComment)) {
//                apiModelParamsList.add(String.format("value=\"%s\"", columnComment));
//            }
//            if (required) {
//                apiModelParamsList.add("required=true");
//            }
//            //根据字段类型 自定义example
//            String resultExample=customExampleByJavaType(item);
//            if(resultExample!=null){
//                apiModelParamsList.add(resultExample);
//            }
//            apiModel.append(StringUtils.join(apiModelParamsList, ", "));
//            apiModel.append(")");
//            readServiceLineBuilder.append(apiModel);
//            readServiceLineBuilder.append(separator);
//            //2. 写入@Nullable
//            if (required) {
//                readServiceLineBuilder.append(String.format("\t@NotNull(message = \"%s不能为空\")", columnComment));
//                readServiceLineBuilder.append(separator);
//            }
//            //3. 写入@Min
////                    if ("id".equals(columnName)) {
////                        readServiceLineBuilder.append("\t@Min(value=0,message = \"id不能小于0\")");
////                        readServiceLineBuilder.append(separator);
////                    }
//            //4. 写入@JsonFormat;
//            String dataType = item.getInfo().get(2);
//            switch (dataType) {
//                case "Date":
//                case "Timestamp":
//                    readServiceLineBuilder.append("\t@JsonFormat(pattern = \"yyyy-MM-dd HH:mm:ss\")");
//                    readServiceLineBuilder.append(separator);
//                    break;
////                case "date":
////                    readServiceLineBuilder.append("\t@JsonFormat(pattern = \"yyyy-MM-dd\")");
////                    readServiceLineBuilder.append(separator);
////                    break;
////                case "year":
////                    readServiceLineBuilder.append("\t@JsonFormat(pattern = \"yyyy\")");
////                    readServiceLineBuilder.append(separator);
////                    break;
////                case "time":
////                    readServiceLineBuilder.append("\t@JsonFormat(pattern = \"HH:mm:ss\")");
////                    readServiceLineBuilder.append(separator);
////                    break;
//            }
//            //5. 写入private String test;
//            readServiceLineBuilder.append(String.format("\tprivate %s %s;", dataType, startLower));
//            readServiceLineBuilder.append(separator);
//            readServiceLineBuilder.append(separator);
//        });
//    }
//
//    @Override
//    public String customExampleByJavaType(ServiceParamsConfig item) {
//        String dataType = item.getInfo().get(2);
//        String comment = item.getInfo().get(1);
//        String name = item.getInfo().get(0);
//        List<String> intList = Arrays.asList("Integer", "Long");
//        if ("Boolean".equals(dataType)) {
//            return String.format("example=\"%s\"", RandomUtil.getRandomElement("true", "false"));
//        } else if (intList.contains(dataType)) {
//            if (comment.contains("【") || comment.contains("[")) {
//                Integer randomNumFromString = RandomUtil.getRandomNumFromString(comment);
//                return String.format("example=\"%d\"", randomNumFromString == null ? 1 : randomNumFromString);
//            } else if (name.contains("age")) {
//                return String.format("example=\"%d\"", RandomUtil.getRandomAge());
//            } else {
//                return String.format("example=\"%d\"", RandomUtil.getRandomNum(100, 5000));
//            }
//        } else if ("String".equals(dataType)) {
//            if (name.contains("name")) {
//                return String.format("example=\"%s\"", RandomUtil.getRandomName());
//            } else if (name.contains("phone") || name.contains("mobile")) {
//                return String.format("example=\"%s\"", RandomUtil.getRandomPhone());
//            } else if (name.contains("email")) {
//                return String.format("example=\"%s\"", RandomUtil.getRandomEmail());
//            } else {
//                return "example=\"xxx\"";
//            }
//        }
////        else if ("Date".equals(dataType)) {
////            return String.format("example=\"%s\"", DateUtil.DatetoStringFormat(RandomUtil.getRandomDate(), "yyyy"));
////        } else if ("time".equals(dataType)) {
////            return String.format("example=\"%s\"", DateUtil.DatetoStringFormat(RandomUtil.getRandomDate(), "HH:mm:ss"));
////        }
//        else if ("Time".equals(dataType)) {
//            return String.format("example=\"%s\"", DateUtil.DatetoStringFormat(RandomUtil.getRandomDate(), "yyyy-MM-dd"));
//        } else if ("Date".equals(dataType) || "datetime".equals(dataType)) {
//            return String.format("example=\"%s\"", DateUtil.DatetoStringFormat(RandomUtil.getRandomDate(), "yyyy-MM-dd HH:mm:ss"));
//        }else{
//            return null;
//        }
//    }
//
//    @Override
//    public void writeJavaVariableByDbType(StringBuilder readServiceLineBuilder, String separator, List<ColumnEntity> columnEntityList) {
//        columnEntityList.forEach(item -> {
//            String columnName = item.getColumnName();
//            String columnComment = item.getColumnComment();
//            String startLower = StringUtil.startLower(StringUtil.lineToHump(columnName));
//            StringBuilder apiModel = new StringBuilder();
//            List<String> apiModelParamsList = new ArrayList<>();
//
//            //1. 写入@ApiModelProperty
//            apiModel.append("\t@ApiModelProperty(");
//            if (!StringUtils.isEmpty(columnComment)) {
//                apiModelParamsList.add(String.format("value=\"%s\"", columnComment));
//            }
//            if ("NO".equals(item.getNullable())) {
//                apiModelParamsList.add("required=true");
//            }
//            //根据字段类型 自定义example
//            customExampleByDbType(item, apiModelParamsList);
//            apiModel.append(StringUtils.join(apiModelParamsList, ","));
//            apiModel.append(")");
//            readServiceLineBuilder.append(apiModel);
//            readServiceLineBuilder.append(separator);
//            //2. 写入@Nullable
//            if ("NO".equals(item.getNullable())) {
//                readServiceLineBuilder.append(String.format("\t@NotNull(message = \"%s不能为空\")", columnComment));
//                readServiceLineBuilder.append(separator);
//            }
//            //3. 写入@Min
//            if ("id".equals(columnName)) {
//                readServiceLineBuilder.append("\t@Min(value=0,message = \"id不能小于0\")");
//                readServiceLineBuilder.append(separator);
//            }
//            //4. 写入@JsonFormat;
//            String dataType = item.getDataType();
//            switch (dataType) {
//                case "datetime":
//                case "timestamp":
//                    readServiceLineBuilder.append("\t@JsonFormat(pattern = \"yyyy-MM-dd HH:mm:ss\")");
//                    readServiceLineBuilder.append(separator);
//                    break;
//                case "date":
//                    readServiceLineBuilder.append("\t@JsonFormat(pattern = \"yyyy-MM-dd\")");
//                    readServiceLineBuilder.append(separator);
//                    break;
//                case "year":
//                    readServiceLineBuilder.append("\t@JsonFormat(pattern = \"yyyy\")");
//                    readServiceLineBuilder.append(separator);
//                    break;
//                case "time":
//                    readServiceLineBuilder.append("\t@JsonFormat(pattern = \"HH:mm:ss\")");
//                    readServiceLineBuilder.append(separator);
//                    break;
//            }
//            //5. 写入private String test;
//            String javaTypeByDbType = getJavaTypeByDbType(item.getDataType());
//            readServiceLineBuilder.append(String.format("\tprivate %s %s;", javaTypeByDbType, startLower));
//            readServiceLineBuilder.append(separator);
//            readServiceLineBuilder.append(separator);
//        });
//    }
//
//    @Override
//    public String getJavaTypeByDbType(String dataType) {
//        String result;
//        if (dataType.contains("char") || dataType.contains("text")) {
//            result = "String";
//        } else if (dataType.contains("blob")) {
//            result = "byte[]";
//        } else {
//            switch (dataType) {
//                case "bigint":
//                    result = "Long";
//                    break;
//                case "tinyint":
//                case "bit":
//                    result = "Boolean";
//                    break;
//                case "smallint":
//                case "mediumint":
//                case "int":
//                    result = "Integer";
//                    break;
//                case "float":
//                    result = "Float";
//                    break;
//                case "double":
//                    result = "Double";
//                    break;
//                case "decimal":
//                    result = "BigDecimal";
//                    break;
//                case "date":
//                case "year":
//                case "timestamp":
//                case "datetime":
//                    result = "Date";
//                    break;
//                case "time":
//                    result = "Time";
//                    break;
//                default:
//                    throw new RuntimeException("cannot find the javaType: " + dataType);
//            }
//        }
//        return result;
//
//    }
//
//
//    @Override
//    public void customExampleByDbType(ColumnEntity item, List<String> apiModelParamsList) {
//        String dataType = item.getDataType();
//        String comment = item.getColumnComment();
//        String name = item.getColumnName();
//        if ("tinyint".equals(dataType)) {
//            apiModelParamsList.add(String.format("example=\"%s\"", RandomUtil.getRandomElement("true", "false")));
//        } else if (dataType.contains("int")) {
//            if (comment.contains("【") || comment.contains("[")) {
//                Integer randomNumFromString = RandomUtil.getRandomNumFromString(comment);
//                apiModelParamsList.add(String.format("example=\"%d\"", randomNumFromString == null ? 1 : randomNumFromString));
//            } else if (name.contains("age")) {
//                apiModelParamsList.add(String.format("example=\"%d\"", RandomUtil.getRandomAge()));
//            } else {
//                apiModelParamsList.add(String.format("example=\"%d\"", RandomUtil.getRandomNum(100, 5000)));
//            }
//        } else if (dataType.contains("char")) {
//            if (name.contains("name")) {
//                apiModelParamsList.add(String.format("example=\"%s\"", RandomUtil.getRandomName()));
//            } else if (name.contains("phone") || name.contains("mobile")) {
//                apiModelParamsList.add(String.format("example=\"%s\"", RandomUtil.getRandomPhone()));
//            } else if (name.contains("email")) {
//                apiModelParamsList.add(String.format("example=\"%s\"", RandomUtil.getRandomEmail()));
//            } else {
//                apiModelParamsList.add("example=\"xxx\"");
//            }
//        } else if ("year".equals(dataType)) {
//            apiModelParamsList.add(String.format("example=\"%s\"", DateUtil.DatetoStringFormat(RandomUtil.getRandomDate(), "yyyy")));
//        } else if ("time".equals(dataType)) {
//            apiModelParamsList.add(String.format("example=\"%s\"", DateUtil.DatetoStringFormat(RandomUtil.getRandomDate(), "HH:mm:ss")));
//        } else if ("date".equals(dataType)) {
//            apiModelParamsList.add(String.format("example=\"%s\"", DateUtil.DatetoStringFormat(RandomUtil.getRandomDate(), "yyyy-MM-dd")));
//        } else if ("timestamp".equals(dataType) || "datetime".equals(dataType)) {
//            apiModelParamsList.add(String.format("example=\"%s\"", DateUtil.DatetoStringFormat(RandomUtil.getRandomDate(), "yyyy-MM-dd HH:mm:ss")));
//        }
//    }
//
//    @Override
//    public void importTypeDefinitionByDbType(StringBuilder readServiceLineBuilder, List<String> distinctTableDataTypeList, List<ColumnEntity> columnEntityList) {
//        String separator = System.getProperty("line.separator");
//        //有没有id
//        ColumnEntity columnIdEntity = columnEntityList.stream().filter(item -> "id".equals(item.getColumnName())).findFirst().orElse(null);
//        if (columnIdEntity != null) {
//            readServiceLineBuilder.append("import javax.validation.constraints.Min;");
//            readServiceLineBuilder.append(separator);
//        }
//        //有没有nullable=NO
//        ColumnEntity columnNullableEntity = columnEntityList.stream().filter(item -> "NO".equals(item.getNullable())).findFirst().orElse(null);
//        if (columnNullableEntity != null) {
//            readServiceLineBuilder.append("import javax.validation.constraints.NotNull;");
//            readServiceLineBuilder.append(separator);
//        }
//        //数据类型判断
//        if (distinctTableDataTypeList.contains("decimal")) {
//            readServiceLineBuilder.append("import java.math.BigDecimal;");
//            readServiceLineBuilder.append(separator);
//        }
//        if (distinctTableDataTypeList.contains("date") || distinctTableDataTypeList.contains("timestamp") || distinctTableDataTypeList.contains("year")) {
//            readServiceLineBuilder.append("import java.util.Date;");
//            readServiceLineBuilder.append(separator);
//            readServiceLineBuilder.append("import com.fasterxml.jackson.annotation.JsonFormat;");
//            readServiceLineBuilder.append(separator);
//        }
//        if (distinctTableDataTypeList.contains("datetime") || distinctTableDataTypeList.contains("time")) {
//            readServiceLineBuilder.append("import java.sql.Time;");
//            readServiceLineBuilder.append(separator);
//            readServiceLineBuilder.append("import com.fasterxml.jackson.annotation.JsonFormat;");
//            readServiceLineBuilder.append(separator);
//        }
//        readServiceLineBuilder.append(separator);
//    }
//
//
//    //写入模板文件   0 service 1 serviceImpl  2 mapper 3 mapperResources 4 com.saidake.citi.controller
//    @Override
//    public void writeTemplateFile(String writeType, BufferedReader bufferedReader,
//                                  int readServiceFileAheadLimit,
//                                  BufferedWriter bufferedWriter,
//                                  String currentTableServiceFilePath,
//
//                                  TableEntity tableEntity,
//                                  ServiceTableConfig currentServiceTableConfig
//    ) {
//
//        if ("com.saidake.citi.controller".equals(writeType)
//                && generatorProperties.getDb() != null
//                && generatorProperties.getDb().getControllerPassTables() != null
//                && generatorProperties.getDb().getControllerPassTables().contains(tableEntity.getTableName())
//        ) {
//            try {
//                bufferedReader.reset();
//                bufferedWriter.close();
//                log.info("pass com.saidake.citi.controller table: {}", tableEntity.getTableName());
//
//            } catch (IOException e) {
//                e.printStackTrace();
//            }
//            return;
//        }
//        ParamsConfig params = generatorProperties.getParams();
//        String currentLineTableName = tableEntity.getTableName();
//        String currentTableComment = tableEntity.getTableComment();
//        String currentTableName = StringUtil.lineToHump(tableEntity.getTableName());
//        String currentStartUpperTableName = StringUtil.startUpper(currentTableName);
//        String currentStartLowerTableName = StringUtil.startLower(currentTableName);
//        String dtoRequestPackage = generatorProperties.getCore().getDtoRequestPackage();
//        Boolean hasDtoTableFolder = generatorProperties.getIo().getHasDtoTableFolder();
//        String dtoRequestSuffix = generatorProperties.getSuffix().getDtoRequestSuffix();
//        String separator = System.getProperty("line.separator");
//        try {
//            bufferedReader.mark(readServiceFileAheadLimit);
//            String readServiceLine = bufferedReader.readLine();
//
//            int enterTime = 0;
//            boolean hasEnter = false;
//            //START//========================================================================= 遍历每一行
//            while (readServiceLine != null) {
//                Integer count1 = StringUtil.countSubString(readServiceLine, "{");
//                Integer count2 = StringUtil.countSubString(readServiceLine, "}");
//                if (count1 > 0) {
//                    enterTime += count1;
//                    hasEnter = true;
//                }
//                if (count2 > 0) {
//                    enterTime -= count2;
//                }
//                List<String> checkList = Arrays.asList("com.saidake.citi.controller", "service", "serviceImpl");
//                //在开头import
//                if (checkList.contains(writeType) && readServiceLine.startsWith("package ") && currentServiceTableConfig != null && currentServiceTableConfig.getSpecial() != null) {
//                    StringBuilder readServiceLineBuilderStartPackage = new StringBuilder(readServiceLine);
//                    readServiceLineBuilderStartPackage.append(separator);
//                    List<ServiceMethodConfig> special = currentServiceTableConfig.getSpecial();
//                    special.forEach(item -> {
//                        String currentLoginName = item.getName().get(0);
//                        String currentPackage;
//                        String currentLoginRequestName = currentStartUpperTableName + StringUtil.startUpper(currentLoginName) + dtoRequestSuffix;
//                        if (hasDtoTableFolder) {
//                            currentPackage = dtoRequestPackage + "." + currentStartLowerTableName;
//                        } else {
//                            currentPackage = dtoRequestPackage;
//                        }
//                        readServiceLineBuilderStartPackage.append(String.format("import %s.%s;", currentPackage, currentLoginRequestName));
//                        readServiceLineBuilderStartPackage.append(separator);
//                    });
//                    bufferedWriter.write(readServiceLineBuilderStartPackage.toString());
//                }
//                //在末尾，包含特殊服务时，com.saidake.citi.controller 写特殊服务
//                else if (hasEnter && enterTime == 0) {
//                    if (checkList.contains(writeType) && currentServiceTableConfig != null && currentServiceTableConfig.getSpecial() != null) {
//                        bufferedReader.readLine(); //跳过}
//                        StringBuilder readServiceLineBuilderEnd = new StringBuilder(separator);
//                        List<ServiceMethodConfig> serviceMethodConfigList = currentServiceTableConfig.getSpecial();
//                        serviceMethodConfigList.forEach(item -> writeMethod(writeType, currentStartUpperTableName, currentStartLowerTableName, readServiceLineBuilderEnd, item));
//                        readServiceLineBuilderEnd.append(separator);
//                        readServiceLineBuilderEnd.append("}");
//                        bufferedWriter.write(readServiceLineBuilderEnd.toString());
//                        break;
//                    }
//                    bufferedWriter.write(readServiceLine);  //大写表名替换
//                    bufferedWriter.write(separator);
//                } else {
//                    //对当前行进行 字符串替换
//                    readServiceLine = readServiceLine.replace(params.getStartUpperTemplateTableName(), currentStartUpperTableName);  //大写表名替换
//                    readServiceLine = readServiceLine.replace(params.getStartLowerTemplateTableName(), currentStartLowerTableName);  //小写表名替换
//                    if ("mapperResources".equals(writeType))
//                        readServiceLine = readServiceLine.replace(generatorProperties.getTemplate().getTableName(), currentLineTableName);    //下划线表名替换【mapper resources】
//                    if ("com.saidake.citi.controller".equals(writeType)) {
//                        readServiceLine = readServiceLine.replace(generatorProperties.getTemplate().getTableComment(), currentTableComment);  //表注释替换【com.saidake.citi.controller】
//                        if (readServiceLine.contains("接口"))
//                            readServiceLine = readServiceLine.replace("表", "");  //表注释替换【com.saidake.citi.controller】
//                    }
//                    bufferedWriter.write(readServiceLine);  //大写表名替换
//                    bufferedWriter.write(separator);
//                }
//                readServiceLine = bufferedReader.readLine();
//            }
//            bufferedReader.reset();
//            bufferedWriter.close();
//        } catch (IOException ex) {
//            CheckUtil.throwErrorMessage("写入模板文件失败: " + currentTableServiceFilePath);
//        }
//        log.info("写入模板文件成功: {}", currentTableServiceFilePath);
//    }
//
//    @Override
//    public void writeMethod(String writeType, String currentStartUpperTableName, String currentStartLowerTableName, StringBuilder readServiceLineBuilderEnd, ServiceMethodConfig item) {
//        // 1. 公共参数
//        String dtoRequestSuffix = generatorProperties.getSuffix().getDtoRequestSuffix();
//        String dtoResponseSuffix = generatorProperties.getSuffix().getDtoResponseSuffix();
//        String methodSuffix = generatorProperties.getSuffix().getMethodSuffix();
//        String serviceSuffix = generatorProperties.getSuffix().getServiceSuffix();
//        String separator = System.getProperty("line.separator");
//
//        String login = item.getName().get(0);
//        String loginStartUpper = StringUtil.startUpper(login);
//        String loginComment = item.getName().get(1);
//        String method = item.getMethod();
//        String methodStartUpper = StringUtil.startUpper(method);
//        List<ServiceParamsConfig> itemRequest = item.getRequest();
//        List<ServiceParamsConfig> itemResponse = item.getResponse();
//        String paramNameRequestLower = StringUtils.join(currentStartLowerTableName, loginStartUpper, dtoRequestSuffix);
//        String paramNameRequestUpper = StringUtils.join(currentStartUpperTableName, loginStartUpper, dtoRequestSuffix);
//        String paramNameResponseUpper = StringUtils.join(currentStartUpperTableName, loginStartUpper, dtoResponseSuffix);
//
//        // 2. controller的话，添加@RequestMapping和@ApiOperation
//        if ("com.saidake.citi.controller".equals(writeType)) {
//            readServiceLineBuilderEnd.append(String.format("\t@%sMapping(\"/%s\")", methodStartUpper, login));
//            readServiceLineBuilderEnd.append(separator);
//            readServiceLineBuilderEnd.append(String.format("\t@ApiOperation(value = \"业务--%s\")", loginComment));
//            readServiceLineBuilderEnd.append(separator);
//        }
//        // 3. 确定方法开头 返回值  Result或者Result<>
//       String publicStr = "service".equals(writeType) ? "" : "public ";
//        if (itemResponse != null && itemResponse.size() > 0) {
//            readServiceLineBuilderEnd.append(String.format("\t%sResult<%s> %s%s(", publicStr, paramNameResponseUpper, login, methodSuffix));
//        } else {
//            readServiceLineBuilderEnd.append(String.format("\t%sResult %s%s(", publicStr, login, methodSuffix));
//        }
//        // 4.1 为 service 接口添加参数定义
//        if ("service".equals(writeType)) {
//            //直接写定义
//            if ("post".equals(method) && itemRequest != null && itemRequest.size() > 0) {
//                readServiceLineBuilderEnd.append(String.format("%s %s);", paramNameRequestUpper, paramNameRequestLower));
//            } else {
//                List<String> functionParamListByLoginParams = getFunctionParamListByLoginParams(itemRequest,null,false,true);
//                readServiceLineBuilderEnd.append(StringUtils.join(functionParamListByLoginParams,","));
//                readServiceLineBuilderEnd.append(");");
//            }
//            readServiceLineBuilderEnd.append(separator);
//        // 4.2 为 serviceImpl 添加参数定义
//        } else if ("serviceImpl".equals(writeType)) {
//            //直接写定义
//            if ("post".equals(method) && itemRequest != null && itemRequest.size() > 0) {
//                readServiceLineBuilderEnd.append(String.format("%s %s);", paramNameRequestUpper, paramNameRequestLower));
//            } else {
//                List<String> functionParamListByLoginParams = getFunctionParamListByLoginParams(itemRequest,null,false,true);
//                readServiceLineBuilderEnd.append(StringUtils.join(functionParamListByLoginParams,","));
//                readServiceLineBuilderEnd.append("){");
//            }
//            readServiceLineBuilderEnd.append(separator);
//            readServiceLineBuilderEnd.append("\t\treturn null;");
//            readServiceLineBuilderEnd.append(separator);
//            readServiceLineBuilderEnd.append("\t}");
//            readServiceLineBuilderEnd.append(separator);
//        // 4.3 为 com.saidake.citi.controller 添加参数定义，并添加调用service和return代码
//        } else if ("com.saidake.citi.controller".equals(writeType)) {
//            //写注解
//            readServiceLineBuilderEnd.append(separator);
//            //根据get post 确定参数
//            if ("post".equals(method) && itemRequest != null && itemRequest.size() > 0) {
//                readServiceLineBuilderEnd.append(String.format("\t@RequestBody @ApiParam(value = \"json请求体，参数对应其每个属性\") @Valid %s%s%s %s) {",
//                        currentStartUpperTableName, loginStartUpper, dtoRequestSuffix, paramNameRequestLower));
//                readServiceLineBuilderEnd.append(separator);
//                readServiceLineBuilderEnd.append(String.format("\t\treturn %s%s.%s%s(%s);", currentStartLowerTableName,serviceSuffix, login, methodSuffix, paramNameRequestLower));
//            // 不是post默认是get了
//            } else {
//                List<String> functionParamListByLoginParams = getFunctionParamListByLoginParams(itemRequest, "\t\t",true,true);
//                readServiceLineBuilderEnd.append(StringUtils.join(functionParamListByLoginParams,","+separator));
//                readServiceLineBuilderEnd.append(") {");
//                readServiceLineBuilderEnd.append(separator);
//                List<String> functionParamListByLoginParamsNoType = getFunctionParamListByLoginParams(itemRequest,null, false,false);
//                readServiceLineBuilderEnd.append(String.format("\t\treturn %s%s.%s%s(%s);", currentStartLowerTableName,serviceSuffix, login, methodSuffix,StringUtils.join(functionParamListByLoginParamsNoType,",")));
//            }
//            readServiceLineBuilderEnd.append(separator);
//            readServiceLineBuilderEnd.append("\t}");
//        }
//        readServiceLineBuilderEnd.append(separator);
//        readServiceLineBuilderEnd.append(separator);
//    }
//
//    @Override
//    public List<String> getFunctionParamListByLoginParams(List<ServiceParamsConfig> itemRequest, String itemPrefix, Boolean needRequestParam, Boolean needType) {
//        return itemRequest.stream().map(requestInfoItem -> {
//            boolean isRequired = "required".equals(requestInfoItem.getInfo().get(3));
//            String fieldName = requestInfoItem.getInfo().get(0);
//            String fieldComment = requestInfoItem.getInfo().get(1);
//            String fieldJavaType = requestInfoItem.getInfo().get(2);
//            String fieldExample = customExampleByJavaType(requestInfoItem);
//            String fieldExampleStr = fieldExample == null ? "" : ", " + customExampleByJavaType(requestInfoItem);
//            String requireStr = isRequired ? "required = false, " : "";
//            if (needRequestParam){
//                return String.format("%s@RequestParam(%svalue = \"%s\") @ApiParam(value = \"%s\"%s) %s %s",itemPrefix, requireStr, fieldName, fieldComment, fieldExampleStr, fieldJavaType, fieldName);
//            }else if(needType){
//                return String.format("%s %s", fieldJavaType, fieldName);
//            }else{
//                return fieldName;
//            }
//        }).collect(Collectors.toList());
//    }
//
//    public static String getTableWriteFilePath(String currentStartUpperTableName, String currentStartLowerTableName,
//                                               String folderPath, String templateSuffix, String fileSuffix, Boolean hasTableFolder, Boolean hasImplFolder) {
//        String currentTableServiceImplFileName = StringUtils.join(currentStartUpperTableName, templateSuffix, fileSuffix);
//        return FileUtil.joinPath(folderPath, hasTableFolder ? currentStartLowerTableName : "", hasImplFolder ? GeneratorProperties.serviceImplFolder : "", currentTableServiceImplFileName);
//    }
//
//    //-------------------------------------------------------------------------------------- 工具方法 -----------------------------------------------------------------------------//
//    @Override
//    public void closeBufferedReader(BufferedReader item3) {
//        try {
//            item3.close();
//        } catch (IOException e) {
//            e.printStackTrace();
//        }
//    }
//
//    //核心写文件逻辑
//    @Override
//    public BufferedWriter createBufferedWriter(String filePath, Boolean existThrowError) {
//        try {
//            File file = new File(filePath);
//            File fileParent = new File(file.getParent());
//            if (!fileParent.exists()) {
//                if (fileParent.mkdirs()) {
//                    log.info("创建父文件夹成功: {}", fileParent.getPath());
//                }
//            }
//            if (existThrowError && file.exists()) {
//                CheckUtil.throwErrorMessage("文件已存在" + filePath);
//            }
//            if (file.createNewFile()) {
//                log.info("[创建] 文件成功: {}", file.getPath());
//            }else{
//                log.info("[重写] 文件: {}", file.getPath());
//            }
//            FileWriter fileWriter = new FileWriter(file);
//            return new BufferedWriter(fileWriter);
//        } catch (IOException e) {
//            e.printStackTrace();
//            return null;
//        }
//    }
//
//    @Override
//    public BufferedReader createBufferedReader(File item) {
//        if (!item.exists()) {
//            CheckUtil.throwErrorMessage("读取文件不存在：" + item);
//        }
//        try {
//            return new BufferedReader(new FileReader(item));
//        } catch (FileNotFoundException e) {
//            e.printStackTrace();
//            return null;
//        }
//    }
//
//    @Override
//    public BufferedReader createBufferedReaderByPath(String filePath) {
//        File file = new File(filePath);
//        if (!file.exists()) {
//            CheckUtil.throwErrorMessage("读取文件不存在：" + filePath);
//        }
//        try {
//            return new BufferedReader(new FileReader(file));
//        } catch (FileNotFoundException e) {
//            e.printStackTrace();
//            return null;
//        }
//    }
//
//}
