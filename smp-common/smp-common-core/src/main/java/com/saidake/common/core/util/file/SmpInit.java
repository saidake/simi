package com.saidake.common.core.util.file;

import com.saidake.common.core.util.file.support.yaml.SmpYmlProperties;
import com.saidake.common.core.util.file.support.yaml.BackupEnum;
import com.saidake.common.core.util.file.support.yaml.ProjectInfo;
import com.saidake.common.core.util.file.support.yaml.WriteInfo;
import com.saidake.common.core.util.file.support.yaml.WriteTypeEnum;

import jakarta.annotation.Nullable;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.CustomClassLoaderConstructor;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Init file util
 *
 * @author Craig Brown
 * @since 1.0.0
 */
@Slf4j
@UtilityClass
public class SmpInit {
    private static final String configPath=Paths.get(System.getProperty("user.home"),".smp").toString();
    private static final String backupPath=Paths.get(configPath,"AAAbackup").toString();
    private static final String configEscapePath=configPath.replaceAll("\\\\","\\\\\\\\");

    private static final String SMP_CONFIG_FILE="smp-init.yml";
    private static final String RP_FILE_SEPARATOR="%%%";
    private static final String BACKUP_SUFFIX=".backup";

    public static Map<String,Set<String>> init() throws IOException {
        Map<String,Set<String>> resultProjectWriteFileMap=new HashMap<>();
        Yaml smpYml=new Yaml(new CustomClassLoaderConstructor(SmpYmlProperties.class,SmpInit.class.getClassLoader()));
        String smpYamlPath = Paths.get(configPath, SMP_CONFIG_FILE).toString();
        log.info("configuration file path: {}",smpYamlPath);
        FileInputStream fileInputStream = new FileInputStream(smpYamlPath);
        SmpYmlProperties smpYmlProperties = smpYml.loadAs(fileInputStream,SmpYmlProperties.class);
        //A.[CORE] write files
        Map<String, Boolean> writeFileMap=new HashMap<>();
        Map<String, Boolean> backupFileMap=new HashMap<>();
        for (ProjectInfo projectInfo : smpYmlProperties.getProject()) {
            log.info("[START] project : {} ==============================",projectInfo.getName());
            Set<String> currentProjectWriteFileSet=new HashSet<>();
            for (WriteInfo writeInfo : projectInfo.getRuleList()) {
                //B. get write file path
                Path writeFilePathObj = Paths.get(projectInfo.getPath(), writeInfo.getWrite());
                String writeFileName = writeFilePathObj.getFileName().toString();
                String writeFilePath = writeFilePathObj.toString();
                if(writeInfo.getOnce()!=null&&writeInfo.getOnce()&&Files.exists(Paths.get(writeFilePath.concat(BACKUP_SUFFIX)))){
                    log.info("write once only, skipping file: {}",writeFilePath);
                    continue;
                }
                String backUpFilePath=null;
                if(writeFileMap.get(writeFilePath)==null){  // The file has not been written.
                    backUpFilePath= createBackupFile(writeInfo.getBackup(), writeFilePath);
                }

                //B. get read file path
                String readFilePath =null;
                if(writeInfo.getRead()!=null){
                    if(writeInfo.getRead().startsWith("/"))readFilePath=replaceProjectInfoString(Paths.get(configPath, writeInfo.getRead()).toString(), projectInfo);
                    else readFilePath=replaceProjectInfoString(Paths.get(writeInfo.getRead()).toString(), projectInfo);
                }
                handleWriteInfo(writeInfo, writeFilePath, backUpFilePath, readFilePath, projectInfo, Boolean.TRUE.equals(writeFileMap.get(writeFilePath)));
                writeFileMap.put(writeFilePath,true);
                currentProjectWriteFileSet.add(writeFileName);
            }
            resultProjectWriteFileMap.put(projectInfo.getName(),currentProjectWriteFileSet);
            log.info("[END  ] project : {} ==============================",projectInfo.getName());
        }
        return resultProjectWriteFileMap;
    }


    private static void handleWriteInfo(WriteInfo writeInfo, String writeFilePath, String backUpFilePath, @Nullable String readFilePath, ProjectInfo projectInfo, boolean isWriteSameFile) throws IOException {
        WriteTypeEnum writeTypeEnum = WriteTypeEnum.fromValue(writeInfo.getType()).orElseThrow(()->new IllegalArgumentException("The type of ruleList must not be null"));
        String resultReadFilePath=isWriteSameFile?writeFilePath:backUpFilePath;
        if(isWriteSameFile)log.info("write the same file: {}",writeFilePath);
        switch (writeTypeEnum){
            case APPEND_PROPERTIES_FOLDER : {
                SmpFileUtils.readAndPutAllPropertiesFromParent(resultReadFilePath, writeFilePath, readFilePath, properties -> replacePropertiesProjectString(projectInfo, properties));
                break;
            }
            case APPEND_PROPERTIES : {
                SmpFileUtils.readAndPutAllProperties(resultReadFilePath, writeFilePath, properties -> replacePropertiesProjectString(projectInfo, properties), readFilePath);
                break;
            }
            case REPLACE_STRING : {
                Map<String, String> keyValueMap =writeInfo.getRpRuleList()!=null?loadRpFile(writeInfo.getRpRuleList()):loadRpFile(readFilePath);
                SmpFileUtils.readAndWriteStringFile(resultReadFilePath, writeFilePath, source->{
                    for (String keyReplace : keyValueMap.keySet()) {
                        String value = keyValueMap.get(keyReplace);
                        String valueResult = replaceProjectInfoString(value, projectInfo);
                        source=source.replaceAll(Pattern.quote(keyReplace), Matcher.quoteReplacement(valueResult));
                    }
                    return source;
                });
                break;
            }
            case APPEND_STRING : {
                Objects.requireNonNull(readFilePath,"readFilePath must not be null");
                String appendString = Files.readString(Paths.get(readFilePath));
                SmpFileUtils.readAndWriteStringFile(resultReadFilePath, writeFilePath, source->source+System.lineSeparator()+appendString);
                break;
            }
            case LINE_REPLACE : {
                Objects.requireNonNull(readFilePath,"readFilePath must not be null");
                Map<String, String> stringStringMap = loadRpFile(readFilePath);
                final Integer[] lineNumber = {0};
                SmpFileUtils.readWriteBackupFile(resultReadFilePath, writeFilePath, null, sourceLine ->{
                    lineNumber[0]++;
                    String value = stringStringMap.get(lineNumber[0].toString());
                    if(value==null)return sourceLine+System.lineSeparator();
                    return replaceProjectInfoString(value, projectInfo)+System.lineSeparator();
                });
                break;
            }
            case LINE_APPEND : {
                Objects.requireNonNull(readFilePath,"readFilePath must not be null");
                Map<String, String> stringStringMap = loadRpFile(readFilePath);
                final Integer[] lineNumber = {0};
                SmpFileUtils.readWriteBackupFile(resultReadFilePath, writeFilePath, null, sourceLine ->{
                    lineNumber[0]++;
                    String value = stringStringMap.get(lineNumber[0].toString());
                    if(value==null)return sourceLine+System.lineSeparator();
                    return sourceLine+System.lineSeparator()+replaceProjectInfoString(value, projectInfo)+System.lineSeparator();
                });
                break;
            }
            // The read file is not necessary.
            case JAVA_ANNOTATION : {
                SmpFileUtils.readWriteBackupFile(resultReadFilePath, writeFilePath, null, sourceLine ->"//"+sourceLine+System.lineSeparator());
                break;
            }

            // The same read file is not necessary.
            case REPLACE_ALL : {
                Objects.requireNonNull(readFilePath,"readFilePath must not be null");
                FileUtils.writeStringToFile(new File(writeFilePath),Files.readString(Paths.get(readFilePath)), StandardCharsets.UTF_8);
                break;
            }
            case XML: {
                SmpXmlUtils.readAndPutAllXml(backUpFilePath,writeFilePath,
                        new StringReader(
                            replaceProjectInfoString(Files.readString(Paths.get(readFilePath)),projectInfo)
                        )
                );
                break;
            }
            default : throw new IllegalStateException("Unexpected value: " + writeInfo.getType());
        }
        log.info("write file completed: {}",writeFilePath);
    }



    private static Properties replacePropertiesProjectString(ProjectInfo projectInfo, Properties properties) {
        properties.forEach((key, value)->{
            value=replaceProjectInfoString((String)value, projectInfo);
            properties.setProperty((String)key, (String)value);
        });
        return properties;
    }


    private static Optional<Object> createBackupFile(Map<String, Boolean> backupFileMap, String backupType, String writeFilePath) throws IOException {
        if(backupType==null)return Optional.empty();
        BackupEnum backupEnum = BackupEnum.fromValue(backupType).orElseThrow(()->new IllegalArgumentException("backup type must not be null: "+writeFilePath));
        switch (backupEnum){
            case CURRENT : {
                String backUpFilePath = writeFilePath.concat(BACKUP_SUFFIX);
                File backupFile = new File(backUpFilePath);
                if(!backupFile.exists())FileUtils.copyFile(new File(writeFilePath), backupFile);
                return Optional.of(backUpFilePath);
            }
            case SMP: {
                String backUpFilePath = Paths.get(backupPath,Paths.get(writeFilePath.concat(BACKUP_SUFFIX)).getFileName().toString()).toString();
                File backupFile = new File(backUpFilePath);
                if(!backupFile.exists())FileUtils.copyFile(new File(writeFilePath), backupFile);
                return Optional.of(backUpFilePath);
            }
            default : throw new IllegalStateException("Unexpected value: " + backupType);
        }
    }


    private static String createBackupFile(String backupType, String writeFilePath) throws IOException {
        BackupEnum backupEnum = BackupEnum.fromValue(backupType).orElseThrow(()->new IllegalArgumentException("backup type must not be null: "+writeFilePath));
        switch (backupEnum){
            case SMP: {
                String backUpFilePath = Paths.get(backupPath,Paths.get(writeFilePath.concat("-"+encryptString(writeFilePath)).concat(BACKUP_SUFFIX)).getFileName().toString()).toString();
                File backupFile = new File(backUpFilePath);
                if(!backupFile.exists())FileUtils.copyFile(new File(writeFilePath), backupFile);
                return backUpFilePath;
            }
            case CURRENT : {
                String backUpFilePath = writeFilePath.concat(BACKUP_SUFFIX);
                File backupFile = new File(backUpFilePath);
                if(!backupFile.exists())FileUtils.copyFile(new File(writeFilePath), backupFile);
                return backUpFilePath;
            }
            default : throw new IllegalStateException("Unexpected value: " + backupType);
        }
    }

    public static String encryptString(String str)  {
        MessageDigest md = null;
        try {
            md = MessageDigest.getInstance("MD5");
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException("MD5 encrypt error");
        }
        md.update(str.getBytes());
        byte[] bt = md.digest();
        if(bt.length<6)throw new IllegalArgumentException("md5 error: "+str);
        return  "" + Character.forDigit(bt[0] & 15, 16)
                + Character.forDigit((bt[1] & 240) >> 4, 16)
                + Character.forDigit(bt[3] & 15, 16)
                + Character.forDigit((bt[4] & 240) >> 4, 16)
                + Character.forDigit(bt[5] & 15, 16);
    }


    private static Map<String,String> loadRpFile(String path) throws IOException {
        List<String> rpRuleList = Files.readAllLines(Paths.get(path));
        return loadRpFile(rpRuleList);
    }


    private static Map<String,String> loadRpFile(List<String> rpRuleList) {
        Map<String, String> resultMap=new HashMap<>();
        for (String property : rpRuleList) {
            int separatorIndex = property.indexOf(Matcher.quoteReplacement(RP_FILE_SEPARATOR));
            if(separatorIndex==-1)throw new IllegalArgumentException("rp rule must contains separator: "+ RP_FILE_SEPARATOR);
            resultMap.put(property.substring(0,separatorIndex),property.substring(separatorIndex+3));
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
