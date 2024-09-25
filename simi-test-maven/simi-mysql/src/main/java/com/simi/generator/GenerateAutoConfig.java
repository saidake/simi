//package com.simi.generator;
//import com.google.common.collect.ImmutableMap;
//import com.simi.initcash.util.FileUtil;
//import org.apache.commons.io.FileUtils;
//import org.apache.commons.lang3.StringUtils;
//
//import java.io.*;
//import java.util.*;
//import java.util.concurrent.atomic.AtomicBoolean;
//import java.util.concurrent.atomic.AtomicReference;
//import java.util.stream.Collectors;
//
//
//public class GenerateAutoConfig {
//    // 目标项目信息
//    private static final String TARGET_ENV = "UAT";
//    private static final String TARGET_ROOT_PATH = "C:\\Users\\hz46873\\Desktop\\DevProject\\cash-mgmt-ms";
//    private static final String TARGET_WRITE_ENV_FILENAME = "application-local.properties";
//
//    // 自动配置项目信息
//    private static final String CURRENT_NAME="sdk-init-cash";
//    private static final String CURRENT_RESOURCES_PATH_PREFIX ="/config/";
//    private static final String CURRENT_ENV_REPLACE_PATH="/config/"+TARGET_ENV+"/replace";               // 替换属性文件的 父目录
//    private static final String CURRENT_ENV_APPEND_PATH="/config/"+TARGET_ENV+"/append";                 // 追加属性文件的 父目录
//    private static final String CURRENT_WRITE_COMMON_APPEND_ENV_FILENAME = "local-append.properties";    //公共追加文件
//
//    private static final String BACKUP_FILE_SUFFIX=".backup";
//
//
//    // 源代码字段
//    private static final String targetJavaPath;
//    private static final String targetResourcesPath;
//
//    // 目标项目读取文件字段
//    private static final File targetReadEnvFile;
//    private static final File targetReadEnvBackupFile;
//    private static final File targetWriteEnvFile;
//
//    // 计算字段
//    private static final String currentRootPath;  //当前项目根目录
//    private static final HashMap<String,String> targetEnvReadFilenameMap = new HashMap<>();  //UAT - xxx.properties  目标项目读取的配置文件
//    private static final List<Properties> targetEnvReplaceCommonPropertiesList = new ArrayList<>();        // 配置文件：公共 替换属性
//    private static final List<Properties> targetEnvReplaceCommonStringPropertiesList = new ArrayList<>();  // 配置文件：公共 替换字符串
//    private static final List<Properties> targetEnvReplacePropertiesList = new ArrayList<>();   // 配置文件： 环境 替换属性
//    //    public static final List<Properties> targetEnvAppendPropertiesList = new ArrayList<>();  // 配置文件：环境 替换字符串
//    private static final List<String> targetEnvAppendContentList = new ArrayList<>();  // 配置文件：环境 追加字符串
//
//    static{
//        // 源代码字段
//        targetJavaPath = joinPath(TARGET_ROOT_PATH, "src", "main", "java");
//        targetResourcesPath = joinPath(TARGET_ROOT_PATH, "src", "main", "resources");
//
//        // 计算字段
//        currentRootPath=System.getProperty("user.dir")+File.separator+CURRENT_NAME;
//        targetEnvReadFilenameMap.put("DEV","application-dev1cloud.properties");
//        targetEnvReadFilenameMap.put("UAT","application-uat1cloud.properties");
//        targetEnvReadFilenameMap.put("UAT2","application-uat2cloud.properties");
//        targetEnvReadFilenameMap.put("PROD","application-uat1cloud.properties");
//        targetEnvReplaceCommonPropertiesList.add(loadProperties("local-replace.properties"));
//        targetEnvReplaceCommonStringPropertiesList.add(loadCommonReplaceStringProperties("local-replace-string.properties"));
//        calculateTargetPropertiesReplaceOrAppend(CURRENT_ENV_REPLACE_PATH,targetEnvReplacePropertiesList);
//        File[] replaceFiles = new File(InitCash.class.getResource(CURRENT_ENV_APPEND_PATH).getPath()).listFiles();
//        for (File replaceFile : replaceFiles) {
//            targetEnvAppendContentList.add(replaceFile.getPath());
//        }
//
//        // 目标项目读取文件字段
//        targetWriteEnvFile = new File(joinPath(targetResourcesPath, TARGET_WRITE_ENV_FILENAME));
//        targetReadEnvFile = new File(joinPath(targetResourcesPath,targetEnvReadFilenameMap.get(TARGET_ENV)));
//        targetReadEnvBackupFile = new File(joinPath(targetResourcesPath, targetEnvReadFilenameMap.get(TARGET_ENV)+BACKUP_FILE_SUFFIX));
//    }
//
//    private static void calculateTargetPropertiesReplaceOrAppend(String currentEnvReplacePath, List<Properties> targetEnvReplacePropertiesList) {
//        File[] replaceFiles = new File(InitCash.class.getResource(currentEnvReplacePath).getPath()).listFiles();
//        for (File currentReplaceFile: replaceFiles){
//            try {
//                Properties properties = loadPropertiesByInputStream(new FileInputStream(currentReplaceFile));
//                targetEnvReplacePropertiesList.add(properties);
//            } catch (FileNotFoundException e) {
//                e.printStackTrace();
//            }
//        }
//    }
//
//    public static void main(String[] args) throws IOException {
//        //A. define common data
//        System.out.println("currentRootPath: "+currentRootPath);
//
//        File bootFile = new File(joinPath(targetResourcesPath, "bootstrap.yml"));
//        File bootBackupFile = new File(joinPath(targetResourcesPath, "bootstrap.yml.backup"));
//
//        File logbackFile = new File(joinPath(targetResourcesPath, "logback.xml"));
//        File logbackBackupFile = new File(joinPath(targetResourcesPath, "logback.xml.backup"));
//
//        File pomFile = new File(joinPath(TARGET_ROOT_PATH, "pom.xml"));
//        File pomBackupFile = new File(joinPath(TARGET_ROOT_PATH, "pom.xml.backup"));
//
//        //A. kafka listener
//        handleRemoveKafkaListener();
//
//        //A. CcbHttpRequestInterceptor
//        handleEditCcbHttpRequestInterceptor();
//
//        //A. create backup file
//        handleCreateBackupFile(bootFile, bootBackupFile, logbackFile, logbackBackupFile, pomFile, pomBackupFile);
//
//        //A. bootstrap.yml
//        handleEditBootstrapYml(bootFile, bootBackupFile);
//
//        //A. logback.xml
//        handleEditLogbackFile(logbackFile, logbackBackupFile);
//
//        //A. application-local.properties
//        handleEditApplicationLocalProperties();
//
//        //A. pom.xml
//        handleEditPomFile(pomFile, pomBackupFile);
//    }
//
//    private static void handleEditCcbHttpRequestInterceptor() throws IOException {
//        String targetPath="C:\\Users\\hz46873\\Desktop\\DevProject\\cash-mgmt-ms\\src\\main\\java\\com\\citi\\ccb\\utils\\http\\CcbHttpRequestInterceptor.java";
//        String uatToken="\"Vl0XZhctAauzlhYyEPHAfX6vtnMgb31l5WsuJGAMxEp0lmKWyJ5ox7XoC23tXwPn\"";
//        File readFile = new File(targetPath);
//        String tempDir = System.getProperty("java.io.tmpdir");
//        File tempFile=new File(tempDir,readFile.getName()+BACKUP_FILE_SUFFIX);
//        System.out.println("tempTargetFile path: "+tempFile.getPath());
//        if(!tempFile.exists()){
//            FileUtils.copyFile(readFile,tempFile);
//            System.out.println("created tempTargetFile: "+tempFile.getPath());
//        }
//        try {
//            FileUtil.readAndWriteFile(
//                    tempFile.getPath(),
//                    targetPath,
//                    ImmutableMap.of("CcbSecurityToken.getInstance().token()",uatToken,"httpServletRequest.getHeader(X_CITIPORTAL_CSRFTOKEN)",uatToken));
//        } catch (IOException e) {
//            e.printStackTrace();
//        }
//    }
//
//    private static void handleEditBootstrapYml(File bootFile, File bootBackupFile) throws IOException {
//        String bootstrapAppendString=readStringResourceFileContent(CURRENT_RESOURCES_PATH_PREFIX +"bootstrap-append.yml");
//        FileUtil.appendContentToFile(bootBackupFile.getPath(), bootFile.getPath(),bootstrapAppendString);
//        FileUtils.copyFile(targetReadEnvBackupFile, targetWriteEnvFile);
//        System.out.println(String.format("copy read file [%s] to [%s]successfully", targetReadEnvBackupFile.getPath(),targetWriteEnvFile.getPath()));
//    }
//
//    private static void handleEditApplicationLocalProperties() {
//        FileUtil.readAndWriteFile(targetReadEnvBackupFile.getPath(),targetWriteEnvFile.getPath(),
//                readStringResourceFileContent(CURRENT_RESOURCES_PATH_PREFIX +CURRENT_WRITE_COMMON_APPEND_ENV_FILENAME)
//                        +System.lineSeparator()
//                        + targetEnvAppendContentList.stream().map(item->readStringFileContent(item)).collect(Collectors.joining(System.lineSeparator())),
//                currentStorageLine -> {
//                    for (Properties properties : targetEnvReplaceCommonStringPropertiesList) {
//                        for (String stringPropertyName : properties.stringPropertyNames()) {
//                            currentStorageLine = currentStorageLine.replace(stringPropertyName, (String)properties.get(stringPropertyName));
//                        }
//                    }
//                    for (Properties properties : targetEnvReplaceCommonPropertiesList) {
//                        currentStorageLine = findAndReplaceWithProperties(currentStorageLine, properties);
//                    }
//                    for (Properties properties : targetEnvReplacePropertiesList) {
//                        currentStorageLine = findAndReplaceWithProperties(currentStorageLine,properties);
//                    }
//                    return currentStorageLine + System.lineSeparator();
//                });
//    }
//
//    private static void handleEditPomFile(File pomFile, File pomBackupFile) {
//        String replaceDependenciesString = readStringResourceFileContent(CURRENT_RESOURCES_PATH_PREFIX +"pom-replace-dependencies.txt");
//        String[] split = replaceDependenciesString.split(System.lineSeparator());
//        List<String> groupIdList=new ArrayList<>();
//        List<String> targetDependenciesList=new ArrayList<>();
//        if(split.length!=0&&split[0].split("\\s+").length>1){
//            Arrays.stream(split).forEach(item -> {
//                String sourceItem = item.split("\\s+")[0];
//                if(sourceItem.startsWith("<groupId>"))groupIdList.add(sourceItem);
//            });
//            targetDependenciesList = Arrays.stream(split).map(item -> item.split("\\s+")[1]).collect(Collectors.toList());
//        }
//
//        //A. pom.xml
//        AtomicReference<Boolean> isEnterDependencies = new AtomicReference<>(false);
//        AtomicReference<Boolean> isEnterDependenciesManagement = new AtomicReference<>(false);
//        AtomicReference<Integer> enterSecretNum = new AtomicReference<>(-1);
//        AtomicReference<Integer> sourceMatchNum = new AtomicReference<>(-1);
//        AtomicReference<Boolean> isDependenciesReadEnd= new AtomicReference<>(false);
//        AtomicReference<Boolean> isEnterPlugins= new AtomicReference<>(false);
//        AtomicReference<Boolean> isEnterBuild= new AtomicReference<>(false);
//
//        List<String> finalTargetDependenciesList = targetDependenciesList;
//        FileUtil.readAndWriteFile(pomBackupFile.getPath(), pomFile.getPath(), null, currentStorageLine -> {
//            // dependencies
//            if(currentStorageLine.contains("<dependencyManagement>")){
//                isEnterDependenciesManagement.set(true);
//            }else if(currentStorageLine.contains("</dependencyManagement>")){
//                isEnterDependenciesManagement.set(false);
//            }
//            if (currentStorageLine.contains("<dependencies>")) {
//                isEnterDependencies.set(true);
//            } else if (currentStorageLine.contains("</dependencies>")&&!isEnterDependenciesManagement.get()) {
//                if(!isDependenciesReadEnd.get()){
//                    String pomString = readStringResourceFileContent(CURRENT_RESOURCES_PATH_PREFIX +"pom-append-dependencies.txt");
//                    currentStorageLine=pomString+System.lineSeparator()+currentStorageLine;
//                }
//                isEnterDependencies.set(false);
//                isDependenciesReadEnd.set(true);
//            }
//            // build
//            if (currentStorageLine.contains("<build>")) {
//                isEnterBuild.set(true);
//            } else if (currentStorageLine.contains("</build>")) {
//                isEnterBuild.set(false);
//            }
//            if (currentStorageLine.contains("<plugins>")) {
//                isEnterPlugins.set(true);
//            } else if (currentStorageLine.contains("</plugins>")) {
//                if(isEnterBuild.get()){
//                    String pomPluginsString = readStringResourceFileContent(CURRENT_RESOURCES_PATH_PREFIX +"pom-append-plugins.txt");
//                    currentStorageLine=pomPluginsString+System.lineSeparator()+currentStorageLine;
//                }
//                isEnterPlugins.set(false);
//            }
//            // dependencies
//            String finalCurrentStorageLine = currentStorageLine;
//            if (isEnterDependencies.get() &&!isEnterDependenciesManagement.get()&& groupIdList.stream().anyMatch(item-> finalCurrentStorageLine.trim().equals(item))) {
//                enterSecretNum.set(0);
//                sourceMatchNum.set(groupIdList.indexOf(finalCurrentStorageLine.trim()));
//            }
//            if (isEnterDependencies.get() &&!isEnterDependenciesManagement.get()&&sourceMatchNum.get()>=0 && enterSecretNum.get() >= 0 && enterSecretNum.get() < 3) {
//                if(split.length!=0&&split[0].split("\\s+").length>1)currentStorageLine ="\t\t\t"+ finalTargetDependenciesList.get(sourceMatchNum.get()*3+enterSecretNum.get());
//                enterSecretNum.set(enterSecretNum.get() + 1);
//                if(enterSecretNum.get()>2){
//                    enterSecretNum.set(-1);
//                    sourceMatchNum.set(-1);
//                }
//            }
//            return currentStorageLine + System.lineSeparator();
//        });
//    }
//
//    private static void handleEditLogbackFile(File logbackFile, File logbackBackupFile) {
//        AtomicBoolean isConfigurationStart= new AtomicBoolean(false);
//        AtomicBoolean isPatternSet= new AtomicBoolean(false);
//        AtomicReference<Boolean> isFilePatternSet= new AtomicReference<>(false);
//        FileUtil.readAndWriteFile(logbackBackupFile.getPath(), logbackFile.getPath(),null, currentStorageLine -> {
//            StringBuilder stringBuilder=new StringBuilder(currentStorageLine);
//            if(isConfigurationStart.get()){
//                stringBuilder.append(System.lineSeparator());
//                stringBuilder.append("\t<conversionRule conversionWord=\"clr\" converterClass=\"org.springframework.boot.logging.logback.ColorConverter\" /> \n" +
//                        "\t<conversionRule conversionWord=\"wex\" converterClass=\"org.springframework.boot.logging.logback.WhitespaceThrowableProxyConverter\" />\n" +
//                        "\t<conversionRule conversionWord=\"wEx\" converterClass=\"org.springframework.boot.logging.logback.ExtendedWhitespaceThrowableProxyConverter\" />\n"
//                );
//                isConfigurationStart.set(false);
//            }
//            if(isPatternSet.get()&&currentStorageLine.contains("<Pattern>")){
//                stringBuilder=new StringBuilder("\t\t\t<Pattern>%clr(%date{yyyy-MM-dd HH:mm:ss.SSS}){blue} %clr(-){faint}%clr(%5level) %clr(${PID:- }){magenta} %clr(---){faint} %clr([%thread]){yellow} %clr(%logger){cyan}%clr(:){faint} %m%n${LOG_EXCEPTION_CONVERSION_WORD:-%wEx}</Pattern>");
//                isPatternSet.set(false);
//            }
//            if(isFilePatternSet.get()&&currentStorageLine.contains("<Pattern>")){
//                stringBuilder=new StringBuilder("\t\t\t<Pattern>%date{yyyy-MM-dd HH:mm:ss.SSS} -%5level ${PID:- } --- [%thread] %logger: %m%n${LOG_EXCEPTION_CONVERSION_WORD:-%wEx}</Pattern>");
//                isFilePatternSet.set(false);
//            }
//            if(currentStorageLine.startsWith("<configuration>"))isConfigurationStart.set(true);
//            if(currentStorageLine.contains("<appender name=\"STDOUT\" class=\"ch.qos.logback.core.ConsoleAppender\">"))isPatternSet.set(true);
//            if(currentStorageLine.contains("class=\"ch.qos.logback.core.rolling.RollingFileAppender\""))isFilePatternSet.set(true);
//            return stringBuilder + System.lineSeparator();
//        });
//    }
//
//    private static void handleCreateBackupFile(File bootFile, File bootBackupFile, File logbackFile, File logbackBackupFile, File pomFile, File pomBackupFile) throws IOException {
//        //A. env backup file
//        System.out.println("create backup file: "+targetReadEnvBackupFile.getPath());
//        FileUtils.copyFile(targetReadEnvFile, targetReadEnvBackupFile);
//        //A. bootstrap.yml backup file
//        if (!bootBackupFile.exists()) {
//            FileUtils.copyFile(bootFile, bootBackupFile);
//        }
//        //A. pom.xml backup file
//        if (!pomBackupFile.exists()) {
//            FileUtils.copyFile(pomFile, pomBackupFile);
//        }
//        //A. logback.xml backup file
//        if (!logbackBackupFile.exists()) {
//            FileUtils.copyFile(logbackFile, logbackBackupFile);
//        }
//    }
//
//    private static void handleRemoveKafkaListener() throws IOException {
//        File kafkaListenerWriteLocalFile = new File(joinPath(targetJavaPath, "com\\citi\\ccb\\cashmgmt\\messagebroker\\consumer\\DataHighwayKafkaConsumer.java"));
//        File kafkaListenerBackupWriteLocalFile = new File(joinPath(targetJavaPath, "com\\citi\\ccb\\cashmgmt\\messagebroker\\consumer\\DataHighwayKafkaConsumerBackup.java"));
//        if(kafkaListenerBackupWriteLocalFile.exists()){
//            FileUtils.copyFile(kafkaListenerWriteLocalFile,kafkaListenerBackupWriteLocalFile);
//        }else{
//            FileUtil.readAndWriteFile(kafkaListenerWriteLocalFile.getPath(),kafkaListenerWriteLocalFile.getPath(), null,currentStorageLine -> "//"+currentStorageLine + System.lineSeparator());
//            FileUtils.copyFile(kafkaListenerWriteLocalFile,kafkaListenerBackupWriteLocalFile);
//        }
//    }
//
//
//    //================================================================================================================ 工具函数
//    /**
//     * 将文件读取成字符串
//     * @param path
//     * @return
//     */
//    public static String readStringFileContent(String path)  {
//        List<String> resultList=new ArrayList<>();
//        try(FileReader fileReader=new FileReader(path);BufferedReader bufferedReader=new BufferedReader(fileReader)){
//            String readLine = bufferedReader.readLine();
//            while (readLine!=null){
//                resultList.add(readLine);
//                readLine = bufferedReader.readLine();
//            }
//        }catch (Exception ex){
//            ex.printStackTrace();
//        }
//        return StringUtils.join(resultList,System.lineSeparator());
//
//    }
//
//    /**
//     * 从资源文件中直接读取字符串
//     * @param resourcePath
//     * @return
//     */
//    public static String readStringResourceFileContent(String resourcePath)  {
//        if(InitCash.class.getResource(resourcePath)==null){
//            System.out.println("cannnot find path: "+resourcePath);
//            return "";
//        }
//        return readStringFileContent(InitCash.class.getResource(resourcePath).getPath());
//    }
//
//
//
//    /**
//     * 获取资源文件流
//     * @param fileName
//     * @return
//     */
//    public static InputStream readResourceFileAsStream(String fileName){
//        return InitCash.class.getResourceAsStream(fileName);
//    }
//
//
//    /**
//     * 找到并替换property
//     * @param currentStorageLine
//     * @param properties
//     * @return
//     */
//    public static String findAndReplaceWithProperties(String currentStorageLine, Properties properties) {
//        String[] split = currentStorageLine.split("=");
//        if (split.length==2) {
//            if(properties.getProperty(split[0])!=null){
//                return split[0]+"="+properties.get(split[0]);
//            }
//        }
//        return currentStorageLine;
//    }
//
//
//    /**
//     * 连接路径，自动检查是否已经带有 "/"
//     * @param pathList
//     * @return
//     */
//    public static String joinPath(String... pathList) {
//        if (pathList.length == 0) {
//            throw new RuntimeException("empty pathList");
//        }
//        if (pathList.length == 1) return pathList[0];
//
//        List<String> resultList = new ArrayList<>();
//        resultList.add(pathList[0]);
//        for (int i = 1; i < pathList.length; i++) {
//            String currentItem = pathList[i];
//            if(currentItem==null){
//                System.out.println(Arrays.asList(pathList));
//            }
//            if (currentItem.length() == 0) continue;
//            String beforeItem = pathList[i - 1];
//            Boolean isBefore = beforeItem.endsWith(File.separator);
//            Boolean isCurrent = currentItem.startsWith(File.separator);
//            if (isBefore && isCurrent) {
//                currentItem = currentItem.substring(1);
//            } else if (!isBefore && !isCurrent) {
//                currentItem = File.separator + currentItem;
//            }
//            resultList.add(currentItem);
//        }
//        return StringUtils.join(resultList.toArray());
//    }
//
//
//
//
//    private static Properties loadProperties(String resourceFileName) {
//        InputStream inputStream = readResourceFileAsStream(CURRENT_RESOURCES_PATH_PREFIX + resourceFileName);
//        return new Properties(){{
//            try {
//                load(inputStream);
//                inputStream.close();
//            } catch (IOException e) {
//                e.printStackTrace();
//            }
//        }};
//    }
//    private static Properties loadPropertiesByInputStream(InputStream inputStream) {
//        return new Properties(){{
//            try {
//                load(inputStream);
//                inputStream.close();
//            } catch (IOException e) {
//                e.printStackTrace();
//            }
//        }};
//    }
//
//    //加载目录下的所有properties文件
//    private static List<Properties> loadDirAllChildProperties(String resourceFileName) {
//        InitCash.class.getResource(CURRENT_RESOURCES_PATH_PREFIX);
////        return new Properties(){{
////            try {
////                load(inputStream);
////                inputStream.close();
////            } catch (IOException e) {
////                e.printStackTrace();
////            }
////        }};
//        return null;
//    }
//
//    private static Properties loadCommonReplaceStringProperties(String resourceFileName) {
//        Properties properties = loadProperties(resourceFileName);
//        String replacedCurrentRootPath = currentRootPath.replaceAll("\\\\", "\\\\\\\\");
//        properties.forEach((key,val)->{
//            properties.replace(key,replacedCurrentRootPath+val);
//        });
//        return properties;
//    }
//}
