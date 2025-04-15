package com.simi.toolkit.core;

import com.simi.common.util.SmpAssert;
import com.simi.common.util.file.SimiFileUtils;
import com.simi.common.util.file.SimiXmlUtils;
import com.simi.toolkit.core.exception.InitException;
import com.simi.toolkit.core.domain.yaml.SimiYmlProperties;
import com.simi.toolkit.core.domain.enums.BackupEnum;
import com.simi.toolkit.core.domain.yaml.ProjectInfo;
import com.simi.toolkit.core.domain.yaml.WriteInfo;
import com.simi.toolkit.core.domain.enums.WriteTypeEnum;

import lombok.AllArgsConstructor;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.CustomClassLoaderConstructor;

import java.io.*;
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
 * @since 1.0
 */
@Slf4j
@UtilityClass
public class SimiInitializer {

    private static final String configPath=Paths.get(System.getProperty("user.home"),".simi").toString();
    private static final String backupPath=Paths.get(configPath,"AAAbackup").toString();
    private static final String configEscapePath=configPath.replaceAll("\\\\","\\\\\\\\");

    private static final String SMP_CONFIG_FILE="simi-initializer.yml";
    private static final String RP_FILE_SEPARATOR="%%%";
    private static final String BACKUP_SUFFIX=".backup";

    @FunctionalInterface
    public interface CollectMenuInfo{
        void collect(ProjectInfo projectInfo, String currentEnv);
    }
    @FunctionalInterface
    public interface GetPomProjectName {
        Optional<String> get() throws InitException;
    }

    public static void renderToolAction(SimiYmlProperties simiYmlProperties, CollectMenuInfo collectMenuInfo) throws FileNotFoundException {
        int collect = (int) simiYmlProperties.getProject().stream().mapToLong(projectInfo -> projectInfo.getEnvList().size()).sum();
        int envActionIndex=0;
        for (int i = 0; i < simiYmlProperties.getProject().size(); i++) {
            ProjectInfo projectInfo = simiYmlProperties.getProject().get(i);
            if(!projectInfo.isEnable())continue;
            Iterator<String> iterator = projectInfo.getEnvList().iterator();
            for (int j = 0; iterator.hasNext(); j++) {
                String currentEnv = iterator.next();
                collectMenuInfo.collect(projectInfo,currentEnv);
                envActionIndex++;
            }
        }
    }

    public static SimiYmlProperties readYmlProperties() throws FileNotFoundException {
        Yaml smpYml=new Yaml();
        String smpYamlPath = Paths.get(configPath, SMP_CONFIG_FILE).toString();
        log.info("configuration file path: {}",smpYamlPath);
        FileInputStream fileInputStream = new FileInputStream(smpYamlPath);
        return smpYml.loadAs(fileInputStream, SimiYmlProperties.class);
    }

    public static Map<String, Set<String>> init(SimiYmlProperties simiYmlProperties, String projectName, String env, GetPomProjectName getPomProjectName) throws IOException, InitException {
        Map<String,Set<String>> resultProjectWriteFileMap=new HashMap<>();
        //A.[CORE] write files
        Map<String, Boolean> writeFileMap=new HashMap<>();
        Map<String, Boolean> backupFileMap=new HashMap<>();
        for (ProjectInfo projectInfo : simiYmlProperties.getProject()) {
            if(!projectName.equals(projectInfo.getName()))continue;
            if(!projectInfo.isEnable())continue;
            log.info("[START] project : {} ==============================",projectInfo.getName());
            Set<String> currentProjectWriteFileSet=new HashSet<>();
            for (WriteInfo writeInfo : projectInfo.getRuleList()) {
                if(writeInfo.getActiveEnvList()!=null&&!writeInfo.getActiveEnvList().contains(env))continue;
                //B. get writeFilePath or backupFilePath
                String projectPath=projectInfo.isPomProjectNameCheck()?getPomProjectName.get().orElse(null):projectInfo.getPath();
                SmpAssert.notNull(projectPath,"project.path must not be null: "+projectInfo.getName());
                Path writeFilePathObj = Paths.get(projectPath, writeInfo.getWrite());
                String writeFileName = writeFilePathObj.getFileName().toString();
                String writeFilePath = writeFilePathObj.toString();
                if(writeInfo.getOnce()!=null&&writeInfo.getOnce()&&Files.exists(Paths.get(writeFilePath.concat(BACKUP_SUFFIX)))){
                    log.info("write once only, skipping file: {}",writeFilePath);
                    continue;
                }
                String backUpFilePath=null;
                if(writeFileMap.get(writeFilePath)==null) {  // The file has not been written.
                    backUpFilePath= createBackupFile(writeInfo.getBackup(), writeFilePath);
                }
                handleWriteInfo(writeInfo, writeFilePath, backUpFilePath,  projectInfo,
                        Boolean.TRUE.equals(writeFileMap.get(writeFilePath)),
                        env,
                        projectPath);
                writeFileMap.put(writeFilePath,true);
                currentProjectWriteFileSet.add(writeFileName);
            }
            resultProjectWriteFileMap.put(projectInfo.getName(),currentProjectWriteFileSet);
            log.info("[END  ] project : {} ==============================",projectInfo.getName());
        }
        return resultProjectWriteFileMap;
    }


    private static void handleWriteInfo(WriteInfo writeInfo, String writeFilePath, String backUpFilePath, ProjectInfo projectInfo,
                                        boolean isWriteSameFile, String currentEnv, String projectPath) throws IOException {
        PathReplacer pathReplacer = new PathReplacer(projectInfo.getName(), projectPath, currentEnv);
        //A. get readFilePath from writeInfo
        String readFilePath =null;
        if(writeInfo.getRead()!=null){
            //B. join project path and read path.
            if(writeInfo.getRead().startsWith("/"))readFilePath= pathReplacer.replace(Paths.get(configPath, writeInfo.getRead()).toString());
            else readFilePath=pathReplacer.replace(
                    Paths.get(writeInfo.getRead()).toString());
        }
        log.info("result read path: {}",readFilePath);
        //A. write type
        WriteTypeEnum writeTypeEnum = WriteTypeEnum.fromValue(writeInfo.getType()).orElseThrow(()->new IllegalArgumentException("The type of ruleList must not be null"));
        String resultBackupFilePath=isWriteSameFile?writeFilePath:backUpFilePath;
        if(isWriteSameFile)log.info("write the same file: {}",writeFilePath);
        switch (writeTypeEnum){
            case APPEND_PROPERTIES_FOLDER : {
                SimiFileUtils.readAndPutAllPropertiesFromParent(resultBackupFilePath, writeFilePath, readFilePath, pathReplacer::replaceProperties);
                break;
            }
            case APPEND_PROPERTIES : {
                SimiFileUtils.readAndPutAllProperties(resultBackupFilePath, writeFilePath, pathReplacer::replaceProperties, readFilePath);
                break;
            }
            case REPLACE_STRING : {
                Map<String, String> keyValueMap =writeInfo.getRpRuleList()!=null?loadRpFile(writeInfo.getRpRuleList()):loadRpFile(readFilePath);
                SimiFileUtils.readAndWriteStringFile(resultBackupFilePath, writeFilePath, source->{
                    for (String keyReplace : keyValueMap.keySet()) {
                        String value = keyValueMap.get(keyReplace);
                        String valueResult = pathReplacer.replace(value);
                        source=source.replaceAll(Pattern.quote(keyReplace), Matcher.quoteReplacement(valueResult));
                    }
                    return source;
                });
                break;
            }
            case APPEND_STRING : {
                Objects.requireNonNull(readFilePath,"readFilePath must not be null");
                String appendString = Files.readString(Paths.get(readFilePath));
                SimiFileUtils.readAndWriteStringFile(resultBackupFilePath, writeFilePath, source->source+System.lineSeparator()+appendString);
                break;
            }
            case LINE_REPLACE : {
                Objects.requireNonNull(readFilePath,"readFilePath must not be null");
                Map<String, String> keyValueMap =writeInfo.getRpRuleList()!=null?loadRpFile(writeInfo.getRpRuleList()):loadRpFile(readFilePath);
                final Integer[] lineNumber = {0};
                SimiFileUtils.readWriteBackupFile(resultBackupFilePath, writeFilePath, null, sourceLine ->{
                    lineNumber[0]++;
                    String value = keyValueMap.get(lineNumber[0].toString());
                    if(value==null)return sourceLine+System.lineSeparator();
                    return pathReplacer.replace(value)+System.lineSeparator();
                });
                break;
            }
            case LINE_APPEND : {
                Objects.requireNonNull(readFilePath,"readFilePath must not be null");
                Map<String, String> keyValueMap =writeInfo.getRpRuleList()!=null?loadRpFile(writeInfo.getRpRuleList()):loadRpFile(readFilePath);
                final Integer[] lineNumber = {0};
                SimiFileUtils.readWriteBackupFile(resultBackupFilePath, writeFilePath, null, sourceLine ->{
                    lineNumber[0]++;
                    String value = keyValueMap.get(lineNumber[0].toString());
                    if(value==null)return sourceLine+System.lineSeparator();
                    return sourceLine+System.lineSeparator()+pathReplacer.replace(value)+System.lineSeparator();
                });
                break;
            }
            // The read file is not necessary.
            case JAVA_ANNOTATION : {
                SimiFileUtils.readWriteBackupFile(resultBackupFilePath, writeFilePath, null, sourceLine ->"//"+sourceLine+System.lineSeparator());
                break;
            }

            // The same read file is not necessary.
            case REPLACE_ALL : {
                Objects.requireNonNull(readFilePath,"readFilePath must not be null");
                FileUtils.writeStringToFile(new File(writeFilePath),Files.readString(Paths.get(readFilePath)), StandardCharsets.UTF_8);
                break;
            }
            case XML: {
                SimiXmlUtils.readAndPutAllXml(backUpFilePath,writeFilePath,
                        new StringReader(
                                pathReplacer.replace(Files.readString(Paths.get(readFilePath)))
                        )
                );
                break;
            }
            default : throw new IllegalStateException("Unexpected value: " + writeInfo.getType());
        }
        log.info("write file completed: {}",writeFilePath);
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
        SmpAssert.isTrue(bt.length>=6,"md5 error: "+str);
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
            SmpAssert.isTrue(separatorIndex!=-1,"rp rule must contains separator: "+ RP_FILE_SEPARATOR);
            resultMap.put(property.substring(0,separatorIndex),property.substring(separatorIndex+3));
        }
        return resultMap;
    }


    @AllArgsConstructor
    static final class PathReplacer {
        private final String projectName;
        private final String projectPath;
        private final String currentEnv;
        private String replace(String path){
            String projectNameRegex="${project.name}";
            String projectEnvRegex="${project.env}";
            String projectPathRegex="${project.path}";
            String configPathRegex="${smp}";
            return path.replaceAll(Pattern.quote(projectNameRegex),projectName)
                    .replaceAll(Pattern.quote(projectEnvRegex),currentEnv)
                    .replaceAll(Pattern.quote(projectPathRegex),Matcher.quoteReplacement(projectPath))
                    .replaceAll(Pattern.quote(configPathRegex),Matcher.quoteReplacement(configEscapePath));
        }
        private  Properties replaceProperties(Properties properties) {
            properties.forEach((key, value)->{
                value=this.replace((String)value);
                properties.setProperty((String)key, (String)value);
            });
            return properties;
        }
    }


}