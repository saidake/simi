package com.saidake.common.core.util.file;

import com.saidake.common.core.util.file.support.SmpYmlProperties;
import com.saidake.common.core.util.file.support.smyml.BackupEnum;
import com.saidake.common.core.util.file.support.smyml.ProjectInfo;
import com.saidake.common.core.util.file.support.smyml.WriteInfo;
import com.saidake.common.core.util.file.support.smyml.WriteTypeEnum;
import jakarta.annotation.Nullable;
import org.apache.commons.io.FileUtils;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.Constructor;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Init file util
 *
 * @since 1.0.0
 * @author Craig Brown
 */
public class SmpInit {
    private static final String configPath=Paths.get(System.getProperty("user.home"),".smp").toString();
    private static final String configEscapePath=configPath.replaceAll("\\\\","\\\\\\\\");

    private static final String SMP_CONFIG_FILE="smp.yml";
    private static final String RP_FILE_SEPARATOR="$$$";
    private static final String BACKUP_SUFFIX=".backup";

    public static void init() throws IOException {
        Yaml smpYml=new Yaml(new Constructor(SmpYmlProperties.class));
        String smpYamlPath = Paths.get(configPath, SMP_CONFIG_FILE).toString();
        FileInputStream fileInputStream = new FileInputStream(smpYamlPath);
        SmpYmlProperties smpYmlProperties = smpYml.loadAs(fileInputStream,SmpYmlProperties.class);
        //A.[CORE] write files
        Map<String, Boolean> writeFileMap=new HashMap<>();
        for (ProjectInfo projectInfo : smpYmlProperties.getProject()) {
            for (WriteInfo writeInfo : projectInfo.getFileList()) {
                //B. get write file path
                String writeFilePath = Paths.get(projectInfo.getPath(), writeInfo.getWrite()).toString();
                String backUpFilePath=null;
                if(writeFileMap.get(writeFilePath)==null){  // The file has not been written.
                    backUpFilePath= createBackupFile(writeInfo.getBackup(), writeFilePath);
                    writeFileMap.put(writeFilePath,true);
                }
                //B. get read file path
                String readFilePath =null;
                if(writeInfo.getRead()!=null){
                    if(writeInfo.getRead().startsWith("/"))readFilePath=replaceProjectInfoString(Paths.get(configPath, writeInfo.getRead()).toString(), projectInfo);
                    else readFilePath=replaceProjectInfoString(Paths.get(writeInfo.getRead()).toString(), projectInfo);
                    System.out.println(readFilePath);
                }
                System.out.println(writeInfo.getRead());
                handleWriteInfo(writeInfo.getType(), writeFilePath, backUpFilePath, readFilePath, projectInfo);
            }
        }
    }


    private static void handleWriteInfo(String type, String writeFilePath, @Nullable String backUpFilePath, @Nullable String readFilePath, ProjectInfo projectInfo) throws IOException {
        WriteTypeEnum writeTypeEnum = WriteTypeEnum.fromValue(type);
        String resultReadFilePath=backUpFilePath==null?writeFilePath:backUpFilePath;
        switch (writeTypeEnum){
            case APPEND_PROPERTIES_FOLDER -> {
                SmpFileUtils.readAndPutAllPropertiesFromParent(resultReadFilePath, writeFilePath, readFilePath, properties -> replacePropertiesProjectString(projectInfo, properties));
            }
            case APPEND_PROPERTIES -> {
                SmpFileUtils.readAndPutAllProperties(resultReadFilePath, writeFilePath, properties -> replacePropertiesProjectString(projectInfo, properties), readFilePath);
            }
            case REPLACE_ALL -> {
                Objects.requireNonNull(readFilePath,"readFilePath must not be null");
                FileUtils.writeStringToFile(new File(writeFilePath),Files.readString(Paths.get(readFilePath)), StandardCharsets.UTF_8);
            }
            case REPLACE_STRING -> {
                Objects.requireNonNull(readFilePath,"readFilePath must not be null");
                Map<String, String> stringStringMap = loadRpFile(readFilePath);
                SmpFileUtils.readAndWriteStringFile(resultReadFilePath, writeFilePath, source->{
                    for (String keyReplace : stringStringMap.keySet()) {
                        String value = stringStringMap.get(keyReplace);
                        String valueResult = replaceProjectInfoString(value, projectInfo);
                        source=source.replaceAll(Pattern.quote(keyReplace), Matcher.quoteReplacement(valueResult));
                    }
                    return source;
                });
            }
            case APPEND_STRING -> {
                Objects.requireNonNull(readFilePath,"readFilePath must not be null");
                String appendString = Files.readString(Paths.get(readFilePath));
                SmpFileUtils.readAndWriteStringFile(resultReadFilePath, writeFilePath, source->source+System.lineSeparator()+appendString);
            }
            case LINE_REPLACE -> {
                Objects.requireNonNull(readFilePath,"readFilePath must not be null");
                Map<String, String> stringStringMap = loadRpFile(readFilePath);
                final Integer[] lineNumber = {0};
                SmpFileUtils.readWriteBackupFile(resultReadFilePath, writeFilePath, null, sourceLine ->{
                    lineNumber[0]++;
                    String value = stringStringMap.get(lineNumber[0].toString());
                    if(value==null)return sourceLine+System.lineSeparator();
                    return replaceProjectInfoString(value, projectInfo)+System.lineSeparator();
                });
            }
            case LINE_APPEND -> {
                Objects.requireNonNull(readFilePath,"readFilePath must not be null");
                Map<String, String> stringStringMap = loadRpFile(readFilePath);
                final Integer[] lineNumber = {0};
                SmpFileUtils.readWriteBackupFile(resultReadFilePath, writeFilePath, null, sourceLine ->{
                    lineNumber[0]++;
                    String value = stringStringMap.get(lineNumber[0].toString());
                    if(value==null)return sourceLine+System.lineSeparator();
                    return sourceLine+System.lineSeparator()+replaceProjectInfoString(value, projectInfo)+System.lineSeparator();
                });
            }
            case JAVA_ANNOTATION -> {
                SmpFileUtils.readWriteBackupFile(resultReadFilePath, writeFilePath, null, sourceLine ->{
                    return "//"+sourceLine+System.lineSeparator();
                });
            }
            case POM -> {
                SmpFileUtils.readAndPutAllPom(backUpFilePath,writeFilePath,readFilePath);
            }
            default -> throw new IllegalStateException("Unexpected value: " + type);
        }
    }



    private static Properties replacePropertiesProjectString(ProjectInfo projectInfo, Properties properties) {
        properties.forEach((key, value)->{
            value=replaceProjectInfoString((String)value, projectInfo);
            properties.setProperty((String)key, (String)value);
        });
        return properties;
    }

    private static String createBackupFile(String backupType, String writeFilePath) throws IOException {
        String backUpFilePath=null;
        backUpFilePath = writeFilePath.concat(BACKUP_SUFFIX);
        File backupFile = new File(backUpFilePath);
        BackupEnum backupEnum = BackupEnum.fromValue(backupType);
        if(backupEnum==null)return null;
        switch (backupEnum){
            case CURRENT -> {
                if(!backupFile.exists())FileUtils.copyFile(new File(writeFilePath), backupFile);
            }
            default -> throw new IllegalStateException("Unexpected value: " + backupType);
        }

        return backUpFilePath;
    }

    private static Map<String,String> loadRpFile(String path) throws IOException {
        List<String> properties = Files.readAllLines(Paths.get(path));
        Map<String, String> resultMap=new HashMap<>();
        for (String property : properties) {
            String[] split = property.split(Matcher.quoteReplacement(RP_FILE_SEPARATOR));
            if(split.length!=2)throw new RuntimeException("RP file properties error");
            resultMap.put(split[0],split[1]);
        }
        return resultMap;
    }


    private static String replaceProjectInfoString(String path, ProjectInfo projectInfo){
        String projectNameRegex="${project.name}";
        String projectEnvRegex="${project.env}";
        String projectPathRegex="${project.path}";
        String configPathRegex="${smp}";
        return path.replaceAll(Pattern.quote(projectNameRegex),projectInfo.getName())
                .replaceAll(Pattern.quote(projectEnvRegex),projectInfo.getEnv())
                .replaceAll(Pattern.quote(projectPathRegex),Matcher.quoteReplacement(projectInfo.getPath()))
                .replaceAll(Pattern.quote(configPathRegex),Matcher.quoteReplacement(configEscapePath));
    }
}
