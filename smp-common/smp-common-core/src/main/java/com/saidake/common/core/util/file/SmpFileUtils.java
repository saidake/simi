package com.saidake.common.core.util.file;

import jakarta.annotation.Nullable;
import lombok.Cleanup;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.dom4j.*;
import org.dom4j.io.SAXReader;
import org.dom4j.io.XMLWriter;
import org.dom4j.tree.DefaultElement;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.function.Function;
import java.util.function.UnaryOperator;

/**
 * The class consists exclusively of static method for reading file or
 * write file.
 *
 * @author Craig Brown
 * @since 1.0
 */
@Slf4j
public class SmpFileUtils {

    /**
     * Reads a source file and write the file content as a parameter to the
     * target file.
     *
     * @param readPath the path of read file。(readPath can be exactly the same as writePath)
     * @param writePath the path of write file
     * @param lambda how to handle read strings
     * @throws IOException if there is an error reading the file.
     */
    public static void readAndWriteStringFile(String readPath, String writePath, UnaryOperator<String> lambda) throws IOException {
        Objects.requireNonNull(writePath,"writePath must not be null");
        Objects.requireNonNull(lambda,"lambda must not be null");
        String readString = Files.readString(Paths.get(readPath));
        Files.writeString(Paths.get(writePath),lambda.apply(readString), StandardCharsets.UTF_8);
    }

    /**
     * The append file content will be merged into the write file.
     *
     * @param readPropertiesPath the path of read file。(readPropertiesPath can be exactly the same as writePropertiesPath)
     * @param writePropertiesPath the path of write file。
     * @param lambda lambda
     * @param appendPath the path of read file
     * @throws IOException if there is an error reading the file.
     */
    public static void readAndPutAllProperties(@Nullable String readPropertiesPath, String writePropertiesPath, @Nullable UnaryOperator<Properties> lambda, String... appendPath) throws IOException {
        Objects.requireNonNull(appendPath,"readPath must not be null");
        Objects.requireNonNull(writePropertiesPath,"writePath must not be null");
        Properties writeProperties=new Properties();
        @Cleanup FileInputStream fileInputStream = new FileInputStream(readPropertiesPath);
        writeProperties.load(fileInputStream);
        Set<String> fileNameList=new HashSet<>();
        for (String appendPathItem : appendPath) {
            Path readPath = Paths.get(appendPathItem);
            fileNameList.add(readPath.getFileName().toString());
            Properties appendProperties=new Properties();
            appendProperties.load(new FileInputStream(appendPathItem));
            if(lambda!=null)appendProperties=lambda.apply(appendProperties);
            writeProperties.putAll(appendProperties);
        }
        @Cleanup FileOutputStream fileOutputStream = new FileOutputStream(writePropertiesPath);
        writeProperties.store(fileOutputStream,"merge the content of "+StringUtils.join(fileNameList,", ")+" file");
    }
    /**
     * The append file content will be merged into the write file.
     *
     * @param readPropertiesPath the path of read file。(readPropertiesPath can be exactly the same as writePropertiesPath)
     * @param writePropertiesPath the path of write file。
     * @param parentPath the path of read file
     * @throws IOException if there is an error reading the file.
     */
    public static void readAndPutAllPropertiesFromParent(@Nullable String readPropertiesPath, String writePropertiesPath, String parentPath, @Nullable UnaryOperator<Properties> lambda) throws IOException {
        Objects.requireNonNull(parentPath,"parentPath must not be null");
        Objects.requireNonNull(writePropertiesPath,"writePath must not be null");
        Properties writeProperties=new Properties();
        writeProperties.load(new FileInputStream(readPropertiesPath));
        Set<String> fileNameList=new HashSet<>();
        File parentFile = new File(parentPath);
        if (!parentFile.exists())throw new RuntimeException("parentPath file doesn't exist");
        if(parentFile.listFiles()==null)return;
        for (File childFile : parentFile.listFiles()) {
            Path readPath = Paths.get(childFile.getPath());
            fileNameList.add(readPath.getFileName().toString());
            Properties appendProperties=new Properties();
            appendProperties.load(new FileInputStream(childFile.getPath()));
            if(lambda!=null)appendProperties=lambda.apply(appendProperties);
            writeProperties.putAll(appendProperties);
        }
        writeProperties.store(new FileOutputStream(writePropertiesPath),"merge the content of "+StringUtils.join(fileNameList,", ")+" file");
    }

    private static String xpathNS(String xpath){
        if(!xpath.startsWith("/"))xpath="p:"+xpath;
        return xpath.replaceAll("(?<=/)[A-z1-9]+","p:$0");
    }

    private static final class NameSpaceCleaner extends VisitorSupport {
        public void visit(Document document) {
            ((DefaultElement) document.getRootElement())
                    .setNamespace(Namespace.NO_NAMESPACE);
            document.getRootElement().additionalNamespaces().clear();
        }
        public void visit(Namespace namespace) {
            if (namespace.getParent() != null) {
                namespace.getParent().remove(namespace);
            }
        }
        public void visit(Attribute node) {
            if (node.toString().contains("xmlns")
                    || node.toString().contains("xsi:")) {
                node.getParent().remove(node);
            }
        }
        public void visit(Element node) {
            if (node instanceof DefaultElement) {
                ((DefaultElement) node).setNamespace(Namespace.NO_NAMESPACE);
                node.additionalNamespaces().clear();
            }
        }
    }

    @SneakyThrows
    public static void readAndPutAllPom(String backupPomPath, String writePomPath, String appendXmlPath)  {
        Objects.requireNonNull(appendXmlPath,"appendXmlPath must not be null");
        Objects.requireNonNull(backupPomPath,"backupPomPath must not be null");
        Objects.requireNonNull(writePomPath,"writePomPath must not be null");
        //A. parse file
        SAXReader saxReader=new SAXReader();
        HashMap<String,String> pomNameSpaceMap=new HashMap();
        Document readPomDocument = saxReader.read(backupPomPath);
        String namespaceURI = ((DefaultElement) readPomDocument.getRootElement()).getNamespaceURI();
        pomNameSpaceMap.put("p",namespaceURI);
        Document appendPomDocument = saxReader.read(appendXmlPath);
        //A. foreach replace in append.xml
        List<Node> replaceNodeList = appendPomDocument.selectNodes("/root/replace");
        List<Node> appendNodeList = appendPomDocument.selectNodes("/root/append");
        for (Node replaceNode : replaceNodeList) {
            if(replaceNode.getNodeType()!=Node.ELEMENT_NODE)continue;
            Element replaceElement=(Element) replaceNode;
            String replaceNodeXpath = replaceElement.attributeValue("xpath");
            XPath replaceNodeXpathSecond = createNSXpath(pomNameSpaceMap, readPomDocument, replaceNodeXpath);
            List<Node> pomCheckParentNodeList= replaceNodeXpathSecond.selectNodes(readPomDocument);
            // if(!pomCheckParentNodeList.isEmpty())pomCheckParentNodeList.get(0).getParent().accept(new NameSpaceCleaner());
            //B. foreach ele in append.xml
            List<Element> elements = replaceElement.elements();
            for (Element element : elements) {
                String eleXpath = element.attributeValue("xpath");
                String eleValue = element.attributeValue("value");
                Element firstElement = element.elements().get(0);
                String eleString = element.asXML();
                //B. foreach parent elements in pom.xml to find the ele
                for (Node pomCheckParentNode : pomCheckParentNodeList) {
                    Element parent = pomCheckParentNode.getParent();
                    if(pomCheckParentNode.getNodeType()!=Node.ELEMENT_NODE)continue;
                    Element pomCheckParentElement=(Element) pomCheckParentNode;
                    XPath eleNsXpath = createNSXpath(pomNameSpaceMap, pomCheckParentElement, eleXpath);
                    Node pomCheck = eleNsXpath.selectSingleNode(pomCheckParentElement);
                    if(pomCheck.getNodeType()!=Node.ELEMENT_NODE)continue;
                    Element pomCheckElement=(Element) pomCheck;
                    String text = pomCheckElement.getText();
                    if(text!=null&&text.equals(eleValue)){
                        pomCheckParentElement.accept(new NameSpaceCleaner());
                        List<Element> parentElementList = parent.elements();
                        DefaultElement clone = (DefaultElement)firstElement.clone();
                        synchronizeNameSpace(clone, readPomDocument);
                        parentElementList.add(parentElementList.indexOf(pomCheckParentNode),clone);
                        parent.remove(pomCheckParentElement);
                    }
                }
            }
        }
        for (Node appendNode : appendNodeList) {
            if(appendNode.getNodeType()!=Node.ELEMENT_NODE)continue;
            Element apppendElement=(Element) appendNode;
            String parentXpathString = apppendElement.attributeValue("parent-xpath");
            XPath parentXpath = createNSXpath(pomNameSpaceMap, readPomDocument, parentXpathString);
            Node pomCheckParentNode= parentXpath.selectSingleNode(readPomDocument);
            Element pomCheckParentElement=(Element) pomCheckParentNode;
            Element firstElement = apppendElement.elements().get(0);
            DefaultElement clone = (DefaultElement)firstElement.clone();
            synchronizeNameSpace(clone, readPomDocument);
            pomCheckParentElement.elements().add(clone);
        }
        @Cleanup FileWriter fileWriterJava = new FileWriter(writePomPath);
        XMLWriter xmlWriter = new XMLWriter(fileWriterJava);
        xmlWriter.write( readPomDocument );
    }

    private static void synchronizeNameSpace(DefaultElement clone, Document readPomDocument) {
        clone.setNamespace(readPomDocument.getRootElement().getNamespace());
        for (Element elementChild : clone.elements()) {
            DefaultElement elementDefault = (DefaultElement)elementChild;
            synchronizeNameSpace(elementDefault, readPomDocument);
        }
    }

    private static XPath createNSXpath(HashMap pomNameSpaceMap, Node readPomDocument, String replaceNodeXpath) {
        XPath replaceNodeXpathSecond = readPomDocument.createXPath(xpathNS(replaceNodeXpath));
        replaceNodeXpathSecond.setNamespaceURIs(pomNameSpaceMap);
        return replaceNodeXpathSecond;
    }


    /**
     * 拼接路径和包名
     * @param path  路径
     * @param packageName   包名
     * @return  拼接路径
     */
    public static String joinPathAndPackage(String path,String... packageName){
        for (String name : packageName) {
            name=name.replace(".",File.separator);
            path=path+File.separator;
            path=path.concat(name);
        }
        return path;
    }


    /**
     * Join multi Path
     * @return result path
     */
    public static String joinPath(String... pathList){
        if (pathList.length==0)throw new RuntimeException("empty pathList");
        if(pathList.length==1)return pathList[0];
        List<String> resultList=new ArrayList<>();
        resultList.add(pathList[0]);
        for (int i = 1; i < pathList.length; i++) {
            String currentItem=pathList[i];
            if(currentItem.length()==0)continue;
            String beforeItem=pathList[i-1];
            Boolean isBefore=beforeItem.endsWith(File.separator);
            Boolean isCurrent=currentItem.startsWith(File.separator);
            if(isBefore&&isCurrent){
                currentItem=currentItem.substring(1);
            }else if(!isBefore&&!isCurrent){
                currentItem=File.separator+currentItem;
            }
            resultList.add(currentItem);
        }
        return StringUtils.join(resultList.toArray());
    }



    private static final String SDK_MARK_TAG="SDK_MARK_TAG";
    private static final String SDK_RETURN_MARK_TAG="SDK_RETURN_MARK_TAG";
    private static final ThreadLocal<Boolean> alreadyMarked=new ThreadLocal<>();
    public static void readWriteBackupFile(String readOrWritePath, @Nullable String writePath, @Nullable String appendContent, Function<String,String> lambda) throws IOException {
        Objects.requireNonNull(readOrWritePath,"readPath must not be null");
        Objects.requireNonNull(lambda,"lambda must not be null");
        File readFile = new File(readOrWritePath);
        File writeFile;
        if(writePath==null){
            File readTempFile = File.createTempFile(readFile.getName(), ".backup");
            org.apache.commons.io.FileUtils.copyFile(readFile,readTempFile);
            readFile=readTempFile;
            writeFile=new File(readOrWritePath);
        }else{
            writeFile = new File(writePath);
        }
        //A. 公共数据
        boolean isSameFile= readOrWritePath.equals(writePath);
        //A. 创建临时文件
        if(isSameFile){
            readFile = File.createTempFile(readFile.getName(), ".backup");  // 临时读取文件
            org.apache.commons.io.FileUtils.copyFile(writeFile,readFile);
        }
        //A. 执行匿名函数
        try {
            BufferedReader bufferedReader = new BufferedReader(new FileReader(readFile));
            BufferedWriter bufferedWriter=new BufferedWriter(new FileWriter(writeFile));
            for ( String currentLine = bufferedReader.readLine();currentLine!=null;currentLine = bufferedReader.readLine()){
                //B. 预定义lambda
                String resultLine = lambda.apply(currentLine);
                if(resultLine==null)continue;
                if(SDK_MARK_TAG.equals(resultLine)){
                    bufferedReader.mark((int)readFile.length()+1);
                    alreadyMarked.set(true);
                }else if(resultLine.startsWith(SDK_RETURN_MARK_TAG)){
                    bufferedReader.reset();
                    alreadyMarked.set(false);
                    bufferedWriter.write(resultLine.substring(SDK_RETURN_MARK_TAG.length()));
                }else if(alreadyMarked.get()==null||Boolean.FALSE.equals(alreadyMarked.get())){
                    bufferedWriter.write(resultLine);
                }
            }
            if(StringUtils.isNotBlank(appendContent))bufferedWriter.write(appendContent);
            if(Boolean.TRUE.equals(alreadyMarked.get())){
                alreadyMarked.set(false);
            }
            bufferedReader.close();
            bufferedWriter.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        //A. 删除临时文件
        if(isSameFile&&!readFile.delete()) {
            throw new RuntimeException("delete file error: "+readFile.getPath());
        }
    }


}
