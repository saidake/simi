package com.simi.generator.service.generator;


import com.simi.generator.model.properties.*;
import com.simi.generator.mybatis.entity.ColumnEntity;
import com.simi.generator.mybatis.entity.TableEntity;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.util.List;
import java.util.Set;

public interface GeneratorTemplate {
    //-------------------------------------------------------------------------------------- 稳定版 -----------------------------------------------------------------------------//
    void generateCommonVoClass();

    //-------------------------------------------------------------------------------------- 核心 -----------------------------------------------------------------------------//
    //清空除了模板文件名开头的 其他所有文件（sdkTemplate开头或 SdkTemplate 的文件或文件夹不被清除）
    void clearOtherFiles();

    //初始化覆盖文件
    void generateInitServicesFiles();


    void generateAppendServices();

    //写入dto request文件
    void writeSpecialDto(
            String writeType,
            BufferedReader bufferedReader,
            int readEntityFileAheadLimit,
            BufferedWriter bufferedWriter,
            String currentTableServiceFilePath,

            ServiceTableConfig currentServiceTableConfig,
            ServiceMethodConfig currentServiceMethodConfig
    );


    //-------------------------------------------------------------------------------------- 业务方法 -----------------------------------------------------------------------------//

    void writeTemplateVoFile(
            CommonVoParamsConfig commonVoParamsConfig,
            BufferedReader bufferedReader, int readAheadLimit, BufferedWriter bufferedWriter,
            List<ServiceParamsConfig> fieldList, String voItemName, String voItemComment, String folder
            );

    //写入entity文件
    void writeTemplateFileCheckInside(
            String writeType,
            BufferedReader bufferedReader,
            int readEntityFileAheadLimit,
            BufferedWriter bufferedWriter,
            String currentTableServiceFilePath,

            TableEntity tableEntity,
            ServiceTableConfig currentServiceTableConfig,
            ServiceMethodConfig currentServiceMethodConfig
    );
    //写入模板文件   0 service 1 serviceImpl  2 mapper 3 mapperResources 4 com.simi.citi.controller
    void writeTemplateFile(String writeType, BufferedReader bufferedReader,
                           int readServiceFileAheadLimit,
                           BufferedWriter bufferedWriter,
                           String currentTableServiceFilePath,

                           TableEntity tableEntity,
                           ServiceTableConfig currentServiceTableConfig
    );


    //写入模板文件   0 service 1 serviceImpl  2 mapper 3 mapperResources 4 com.simi.citi.controller
    void writeTemplateFileAppend(String writeType, List<String> bufferedReaderString,
                                 BufferedWriter bufferedWriter,
                                 String currentWriteFilePath,
                                 ServiceTableConfig currentServiceTableConfig
    );

    void writeCommonVoClass(AppendCommonVoConfig appendCommonVoConfig);




    String customExampleByJavaType(ServiceParamsConfig item);
    //-------------------------------------------------------------------------------------- 业务工具 -----------------------------------------------------------------------------//
    //获取 Java类型，根据数据库类型
    String getJavaTypeByDbType(String dataType);


    //写入 Java类型，根据数据库类型
    void writeJavaVariableByDbType(StringBuilder readServiceLineBuilder, String separator, List<ColumnEntity> columnEntityList);
    //写入 Java类型，根据Java类型
    void writeJavaVariableByJavaType(StringBuilder readServiceLineBuilder, String separator, List<ServiceParamsConfig> loginRequestList);


    //import检测
    void importTypeDefinitionByDbType(StringBuilder readServiceLineBuilder,  List<String> distinctTableDataTypeList, List<ColumnEntity> columnEntityList);
    Set<String> importTypeDefinitionByJavaType(List<ServiceParamsConfig> loginRequestList, Boolean needNotNull);
    void   writeImportSet(StringBuilder stringBuilder, Set<String> stringSet);

    /**
     * 根据special里的method数组，根据请求方式批量import（注意：默认找到dto目录下的表目录，再找表前缀的dto request）
     * @param serviceTableConfig
     * @return
     */
    Set<String>  importPostTypeDefinition(ServiceTableConfig serviceTableConfig);

    /**
     * 自定义ApiModelProperty的example
     * @param item                  列实体
     * @param apiModelParamsList    apiMode的参数列表
     */
    void customExampleByDbType(ColumnEntity item, List<String> apiModelParamsList);

    void writeMethod(String writeType, String currentStartUpperTableName, String currentStartLowerTableName, StringBuilder readServiceLineBuilderEnd, ServiceMethodConfig item);

    /**
     * 根据yml的参数数组，获取 函数参数定义数组
     * @param itemRequest           yml的参数定义
     * @param needRequestParam      是否需要@RequestParam注解
     * @return      返回 参数定义数组
     */
    List<String> getFunctionParamListByLoginParams(List<ServiceParamsConfig> itemRequest, String itemPrefix, Boolean needRequestParam, Boolean needType);

    //-------------------------------------------------------------------------------------- 工具方法 -----------------------------------------------------------------------------//
    void closeBufferedReader(BufferedReader item3);

    //核心写文件逻辑
    BufferedWriter createBufferedWriter(String filePath,Boolean checkExist);

    BufferedReader createBufferedReader(File item);

    BufferedReader createBufferedReaderByPath(String filePath);
}
