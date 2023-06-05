//package com.saidake.common.core.util.file;
//
//import jakarta.annotation.Nullable;
//import lombok.extern.slf4j.Slf4j;
//import org.apache.commons.lang3.StringUtils;
//import org.apache.commons.logging.Log;
//import org.apache.commons.logging.LogFactory;
//import org.springframework.boot.SpringApplication;
//import org.w3c.dom.*;
//import org.xml.sax.SAXException;
//
//import javax.xml.parsers.DocumentBuilder;
//import javax.xml.parsers.DocumentBuilderFactory;
//import javax.xml.parsers.ParserConfigurationException;
//import javax.xml.transform.Transformer;
//import javax.xml.transform.TransformerException;
//import javax.xml.transform.TransformerFactory;
//import javax.xml.transform.dom.DOMSource;
//import javax.xml.transform.stream.StreamResult;
//import java.io.*;
//import java.nio.charset.StandardCharsets;
//import java.nio.file.Files;
//import java.nio.file.Path;
//import java.nio.file.Paths;
//import java.util.*;
//import java.util.function.Function;
//import java.util.function.UnaryOperator;
//import java.util.stream.Stream;
//
///**
// * The class consists exclusively of static method for reading file or
// * write file.
// *
// * @author Craig Brown
// * @since 1.0
// */
//@Slf4j
//public class SmpFileBackupUtils {
//    private static final Log logger = LogFactory.getLog(SpringApplication.class);
//
//    private static String REPLACE_NODE_NAME="replace";
//    private static String APPEND_NODE_NAME="append";
//
//    /**
//     * Reads a source file and write the file content as a parameter to the
//     * target file.
//     *
//     * @param readPath the path of read file
//     * @param writePath the path of write file
//     * @param lambda how to handle read strings
//     * @throws IOException if there is an error reading the file.
//     */
//    public static void readAndWriteStringFile(String readPath, String writePath, UnaryOperator<String> lambda) throws IOException {
//        Objects.requireNonNull(readPath,"readPath must not be null");
//        Objects.requireNonNull(writePath,"writePath must not be null");
//        Objects.requireNonNull(lambda,"lambda must not be null");
//        String readString = Files.readString(Paths.get(readPath));
//        Files.writeString(Paths.get(writePath),lambda.apply(readString), StandardCharsets.UTF_8);
//    }
//
//    /**
//     * The append file content will be merged into the write file.
//     *
//     * @param writePath the path of write file
//     * @param appendPath the path of read file
//     * @throws IOException if there is an error reading the file.
//     */
//    public static void readAndPutAllProperties(String writePath, String... appendPath) throws IOException {
//        Objects.requireNonNull(appendPath,"readPath must not be null");
//        Objects.requireNonNull(writePath,"writePath must not be null");
//        Properties writeProperties=new Properties();
//        writeProperties.load(new FileInputStream(writePath));
//        Set<String> fileNameList=new HashSet<>();
//        for (String appendPathItem : appendPath) {
//            Path readPath = Paths.get(appendPathItem);
//            fileNameList.add(readPath.getFileName().toString());
//            Properties appendProperties=new Properties();
//            appendProperties.load(new FileInputStream(appendPathItem));
//            writeProperties.putAll(appendProperties);
//        }
//        writeProperties.store(new FileOutputStream(writePath),"merge the content of "+StringUtils.join(fileNameList,", ")+" file");
//    }
//
//    public static void readAndPutAllProperties(String writePath, String readParentDirectoryPath) throws IOException {
//        Objects.requireNonNull(readParentDirectoryPath,"parentPath must not be null");
//        Objects.requireNonNull(writePath,"writePath must not be null");
//        Properties writeProperties=new Properties();
//        writeProperties.load(new FileInputStream(writePath));
//        Set<String> fileNameList=new HashSet<>();
//        File parentFile = new File(readParentDirectoryPath);
//        if(!parentFile.isDirectory())log.error("");
//        for (File file : parentFile.listFiles()) {
//            if(!file.isFile())continue;
//            fileNameList.add(file.getName().toString());
//            Properties appendProperties=new Properties();
//            appendProperties.load(new FileInputStream(file.getPath()));
//            writeProperties.putAll(appendProperties);
//        }
//        writeProperties.store(new FileOutputStream(writePath),"merge the content of "+StringUtils.join(fileNameList,", ")+" file");
//    }
//
//    public static void readAndPutAllPom(String writePomPath, String... appendXmlPath) throws IOException, ParserConfigurationException, SAXException, TransformerException {
//        File writePomFile = new File(writePomPath);
//        DocumentBuilderFactory pomDocumentBuilderFactory = DocumentBuilderFactory.newInstance();
//        DocumentBuilder pomDocumentBuilderPom = pomDocumentBuilderFactory.newDocumentBuilder();
//        Document pomDocument = pomDocumentBuilderPom.parse(writePomFile);
//        Node project = pomDocument.getElementsByTagName("project").item(0);
//        Element test = pomDocument.createElement("test");
//        project.appendChild(test);
//        NamedNodeMap attributes = project.getAttributes();
//        TransformerFactory pomTransformerFactory = TransformerFactory.newInstance();
//        Transformer pomTransformer = pomTransformerFactory.newTransformer();
//        DOMSource pomDomSource = new DOMSource(pomDocument);
//        StreamResult pomStreamResult = new StreamResult(writePomFile);
//        pomTransformer.transform(pomDomSource,pomStreamResult);
//        for (String appendXml : appendXmlPath) {
//            File appendXmlFile = new File(appendXml);
//            DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
//            DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
//            Document document = documentBuilder.parse(appendXmlFile);
//            Node root = document.getElementsByTagName("root").item(0);
//            NodeList childNodes = root.getChildNodes();
//            for (int i = 0; i < childNodes.getLength(); i++) {
//                Node item = childNodes.item(i);
//                if(item.getNodeType()!=Node.ELEMENT_NODE)continue;
//                System.out.println(item.getNodeName());
//            }
//        }
//
//    }
//
//
//
//
//
//    private static final String SDK_MARK_TAG="SDK_MARK_TAG";
//    private static final String SDK_RETURN_MARK_TAG="SDK_RETURN_MARK_TAG";
//    private static final ThreadLocal<Boolean> alreadyMarked=new ThreadLocal<>();
//
//        /**
//         * 读写文件，对每一行做操作
//         * 可选功能：根据readAndWriteTheSameFileLambda返回的内容，标记一行，向下获取信息（不写入新行），再返回之前地那一行
//         *         返回 "SDK_MARK_TAG"             标记行
//         *         返回 "SDK_RETURN_MARK_TAGxxx"  返回标记行，同时 写入 标记行 查询信息后的最终内容
//         */
//
//    public static void readWriteBackupFile(String readOrWritePath, @Nullable String writePath, @Nullable String appendContent, Function<String,String> lambda) throws IOException {
//        Objects.requireNonNull(readOrWritePath,"readPath must not be null");
//        Objects.requireNonNull(lambda,"lambda must not be null");
//        File readFile = new File(readOrWritePath);
//        File writeFile;
//        //Assert.isFalse(readFile.exists(),"read file not exist");
//        //A. writePath不存在时，创建临时文件
//        if(writePath==null){
//            File readTempFile = File.createTempFile(readFile.getName(), ".backup");
//            org.apache.commons.io.FileUtils.copyFile(readFile,readTempFile);
//            log.info("created temp file: {}",readTempFile.getPath());
//            readFile=readTempFile;
//            writeFile=new File(readOrWritePath);
//        }else{
//            writeFile = new File(writePath);
//        }
//        //A. 公共数据
//        boolean isSameFile= readOrWritePath.equals(writePath);
//        //A. 创建临时文件
//        if(isSameFile){
//            readFile = File.createTempFile(readFile.getName(), ".backup");  // 临时读取文件
//            org.apache.commons.io.FileUtils.copyFile(writeFile,readFile);
//            log.info("create temp file successfully: {}",readFile.getPath());
//        }
//        //A. 执行匿名函数
//        try {
//            BufferedReader bufferedReader = new BufferedReader(new FileReader(readFile));
//            BufferedWriter bufferedWriter=new BufferedWriter(new FileWriter(writeFile));
//            for ( String currentLine = bufferedReader.readLine();currentLine!=null;currentLine = bufferedReader.readLine()){
//                //B. 预定义lambda
//                String resultLine = lambda.apply(currentLine);
//                if(resultLine==null)continue;
//                if(SDK_MARK_TAG.equals(resultLine)){
//                    bufferedReader.mark((int)readFile.length()+1);
//                    alreadyMarked.set(true);
//                }else if(resultLine.startsWith(SDK_RETURN_MARK_TAG)){
//                    bufferedReader.reset();
//                    alreadyMarked.set(false);
//                    bufferedWriter.write(resultLine.substring(SDK_RETURN_MARK_TAG.length()));
//                }else if(alreadyMarked.get()==null||Boolean.FALSE.equals(alreadyMarked.get())){
//                    bufferedWriter.write(resultLine);
//                }
//            }
//            if(StringUtils.isNotBlank(appendContent))bufferedWriter.write(appendContent);
//            if(Boolean.TRUE.equals(alreadyMarked.get())){
//                log.error("didn't return SDK_RETURN_MARK_TAG");
//                alreadyMarked.set(false);
//            }
//            bufferedReader.close();
//            bufferedWriter.close();
//        } catch (IOException e) {
//            e.printStackTrace();
//        }
//        //A. 删除临时文件
//        if(isSameFile&&!readFile.delete()) {
//            throw new RuntimeException("delete file error: "+readFile.getPath());
//        }else{
//            log.info("delete temp file successfully: {}",readFile.getPath());
//        }
//    }
//
//    /**
//     * 拼接路径和包名
//     * @param path  路径
//     * @param packageName   包名
//     * @return  拼接路径
//     */
//    public static String joinPathAndPackage(String path,String packageName){
//        packageName=packageName.replace(".",File.separator);
//        path=path+File.separator;
//        return path.concat(packageName);
//    }
//
//
//    /**
//     * 拼接多路径
//     * @return
//     */
//    public static String joinPath(String... pathList){
//        if (pathList.length==0){
//            throw new RuntimeException("empty pathList");
//        };
//        if(pathList.length==1)return pathList[0];
//
//        List<String> resultList=new ArrayList<>();
//        resultList.add(pathList[0]);
//        for (int i = 1; i < pathList.length; i++) {
//            String currentItem=pathList[i];
//            if(currentItem.length()==0)continue;
//            String beforeItem=pathList[i-1];
//            Boolean isBefore=beforeItem.endsWith(File.separator);
//            Boolean isCurrent=currentItem.startsWith(File.separator);
//            if(isBefore&&isCurrent){
//                currentItem=currentItem.substring(1);
//            }else if(!isBefore&&!isCurrent){
//                currentItem=File.separator+currentItem;
//            }
//            resultList.add(currentItem);
//        }
//        return StringUtils.join(resultList.toArray());
//    }
//
//    /**
//     * 清空其他文件，除了以 excludeFileOrFolder 开头的
//     * @param sourcePath
//     * @param excludeFileOrFolder
//     */
//    public static void clearOtherFilesStartsWith(String sourcePath,String excludeFileOrFolder){
//        File source =new File(sourcePath);
//        File[] listFiles = source.listFiles();
//        assert listFiles != null;
//        for( File currentFile:listFiles){
//            if(currentFile.getName().startsWith(excludeFileOrFolder))continue;
//            try {
//                deleteFileOrDirectory(currentFile);
//            } catch (IOException e) {
//                e.printStackTrace();
//            }
//        }
//    }
//
//    /**
//     * Clear other files in sourcePath except fileName starts with stringList.
//     *
//     * @param sourcePath
//     * @param stringList
//     */
//    public static void clearOtherFilesStartsWithByList(String sourcePath, List<String> stringList ){
//        File source =new File(sourcePath);
//        File[] listFiles = source.listFiles();
//        if(listFiles==null||listFiles.length==0)return;
//        for( File currentFile:listFiles){
//            if(stringList.stream().anyMatch(item->currentFile.getName().startsWith(item))){
//                continue;
//            }
//            try {
//                deleteFileOrDirectory(currentFile);
//            } catch (IOException e) {
//                e.printStackTrace();
//            }
//        }
//    }
//
//    private static void deleteFileOrDirectory(File file) throws IOException {
//        if(file.isDirectory()){
//            try (Stream<Path> walk = Files.walk(Paths.get(file.getPath()))) {
//                walk.sorted(Comparator.reverseOrder())
//                        .forEach(SmpFileBackupUtils::deleteDirectoryStream);
//            }
//        }else{
//            deleteDirectoryStream(Paths.get(file.getPath()));
//        }
//    }
//
//    private static void deleteDirectoryStream(Path path) {
//        try {
//            Files.delete(path);
//            System.out.printf("删除文件成功：%s%n",path.toString());
//        } catch (IOException e) {
//            System.err.printf("无法删除的路径 %s%n%s", path, e);
//        }
//    }
//
//}
