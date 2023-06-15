package com.saidake.plugin.init.core;

import jakarta.annotation.Nullable;
import lombok.Cleanup;
import lombok.SneakyThrows;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;
import org.dom4j.*;
import org.dom4j.io.SAXReader;
import org.dom4j.io.XMLWriter;
import org.dom4j.tree.DefaultElement;

import java.io.FileWriter;
import java.io.IOException;
import java.io.Reader;
import java.util.*;

@UtilityClass
@Slf4j
public class SmpXmlUtils {

    /**
     * A temporary prefix for xmln xmls file to locate namespace.
     */
    private static final String TEMP_XMLN_PREFIX ="smp";
    private static final String TEMP_XMLN_TAG_PREFIX =TEMP_XMLN_PREFIX+":";

    private static final String XML_REPLACE_XPATH ="/root/replace";
    private static final String XML_REPLACE_ATTRIBUTE_XPATH="xpath";
    private static final String XML_REPLACE_ELE_ATTRIBUTE_XPATH="xpath";
    private static final String XML_REPLACE_ELE_ATTRIBUTE_VALUE="xpath-value";
    private static final String XML_REPLACE_ELE_ATTRIBUTE_APPEND_IF_NOT_EXISTS="append-if-not-exists";
    private static final String XML_APPEND_XPATH ="/root/append";
    private static final String XML_APPEND_ATTRIBUTE_PARENT_XPATH="parent-xpath";

    @SneakyThrows
    public static void readAndPutAllXml(String backupPomPath, String writePomPath, Reader reader)  {
        Objects.requireNonNull(reader,"reader must not be null");
        Objects.requireNonNull(backupPomPath,"backupPomPath must not be null");
        Objects.requireNonNull(writePomPath,"writePomPath must not be null");
        //A. parse file
        SAXReader saxReader=new SAXReader();
        Document readPomDocument = saxReader.read(backupPomPath);
        Optional<HashMap<String, String>> pomNameSpaceMap = createNamespaceMap(readPomDocument);
        Document appendPomDocument = saxReader.read(reader);
        //A. foreach replace in append.xml
        handleAppendXml(writePomPath, readPomDocument, pomNameSpaceMap, appendPomDocument);
    }
    @SneakyThrows
    public static void readAndPutAllXml(String backupPomPath, String writePomPath, String appendXmlPath)  {
        Objects.requireNonNull(appendXmlPath,"appendXmlPath must not be null");
        Objects.requireNonNull(backupPomPath,"backupPomPath must not be null");
        Objects.requireNonNull(writePomPath,"writePomPath must not be null");
        //A. parse file
        SAXReader saxReader=new SAXReader();
        Document readPomDocument = saxReader.read(backupPomPath);
        Optional<HashMap<String, String>> pomNameSpaceMap = createNamespaceMap(readPomDocument);
        Document appendPomDocument = saxReader.read(appendXmlPath);
        //A. foreach replace in append.xml
        handleAppendXml(writePomPath, readPomDocument, pomNameSpaceMap, appendPomDocument);
    }

    private static void handleAppendXml(String writePomPath, Document readPomDocument, Optional<HashMap<String, String>> pomNameSpaceMap, Document appendPomDocument) throws IOException {
        List<Node> replaceNodeList = appendPomDocument.selectNodes(XML_REPLACE_XPATH);
        List<Node> appendNodeList = appendPomDocument.selectNodes(XML_APPEND_XPATH);
        for (Node replaceNode : replaceNodeList) {
            if(replaceNode.getNodeType()!=Node.ELEMENT_NODE)continue;
            Element replaceElement=(Element) replaceNode;
            String attributeReplaceXpath = replaceElement.attributeValue(XML_REPLACE_ATTRIBUTE_XPATH);
            List<Node> pomCheckParentNodeList = selectNodeListWithNSXpath(pomNameSpaceMap.orElse(null), readPomDocument, attributeReplaceXpath);
            //B. foreach ele in append.xml
            List<Element> elements = replaceElement.elements();
            for (Element element : elements) {
                String eleXpath = element.attributeValue(XML_REPLACE_ELE_ATTRIBUTE_XPATH);
                String eleValue = element.attributeValue(XML_REPLACE_ELE_ATTRIBUTE_VALUE);
                Element firstElement = element.elements().get(0);
                boolean findTargetEle=false;
                //B. foreach parent elements in pom.xml to find the ele
                for (Node pomCheckParentNode : pomCheckParentNodeList) {
                    Element parent = pomCheckParentNode.getParent();
                    if(pomCheckParentNode.getNodeType()!=Node.ELEMENT_NODE)continue;
                    Element pomCheckParentElement=(Element) pomCheckParentNode;
                    Node pomCheck =  selectSingleNodeListWithNSXpath(pomNameSpaceMap.orElse(null), pomCheckParentElement, eleXpath);
                    if(pomCheck.getNodeType()!=Node.ELEMENT_NODE)continue;
                    Element pomCheckElement=(Element) pomCheck;
                    String text = pomCheckElement.getText();
                    if(text!=null&&text.equals(eleValue)){
                        log.info("found the target xml element: {} - {}",eleXpath,eleValue);
                        findTargetEle=true;
                        List<Element> parentElementList = parent.elements();
                        DefaultElement clone = (DefaultElement)firstElement.clone();
                        synchronizeNameSpace(clone, readPomDocument);
                        //C. append attributes to the pomCheckElement
                        for (Attribute attribute : element.attributes()) {
                            if(XML_REPLACE_ELE_ATTRIBUTE_XPATH.equals(attribute.getName())
                                    ||XML_REPLACE_ELE_ATTRIBUTE_VALUE.equals(attribute.getName())
                                    || XML_REPLACE_ELE_ATTRIBUTE_APPEND_IF_NOT_EXISTS.equals(attribute.getName())
                            )continue;
                            clone.addAttribute(attribute.getName(),attribute.getValue());
                        }
                        parentElementList.add(parentElementList.indexOf(pomCheckParentNode),clone);
                        parent.remove(pomCheckParentElement);
                    }
                }
                //B. append if element does not exists
                if(!findTargetEle&&"true".equals(element.attributeValue(XML_REPLACE_ELE_ATTRIBUTE_APPEND_IF_NOT_EXISTS))){
                    String attributeReplaceParentXpath = attributeReplaceXpath.replaceAll("/+[A-z1-9]+$", "");
                    Element attributeReplaceParentNode =   (Element)selectSingleNodeListWithNSXpath(pomNameSpaceMap.orElse(null), readPomDocument, attributeReplaceParentXpath);
                    Element elementFirst = element.elements().get(0);
                    DefaultElement clone = (DefaultElement)elementFirst.clone();
                    synchronizeNameSpace(clone, readPomDocument);
                    log.info("No target xml element found, append new element: {} - {}",attributeReplaceParentXpath,clone.getName());
                    attributeReplaceParentNode.elements().add(clone);
                }
            }
        }
        for (Node appendNode : appendNodeList) {
            if(appendNode.getNodeType()!=Node.ELEMENT_NODE)continue;
            Element appendElement=(Element) appendNode;
            String parentXpathString = appendElement.attributeValue(XML_APPEND_ATTRIBUTE_PARENT_XPATH);
            Node pomCheckParentNode =  selectSingleNodeListWithNSXpath(pomNameSpaceMap.orElse(null), readPomDocument, parentXpathString);
            Element pomCheckParentElement=(Element) pomCheckParentNode;
            Element elementFirst = appendElement.elements().get(0);
            DefaultElement clone = (DefaultElement)elementFirst.clone();
            synchronizeNameSpace(clone, readPomDocument);
            pomCheckParentElement.elements().add(clone);
        }
        @Cleanup FileWriter fileWriterJava = new FileWriter(writePomPath);
        XMLWriter xmlWriter = new XMLWriter(fileWriterJava);
        xmlWriter.write(readPomDocument);
    }

    private static String xpathNS(String xpath){
        if(!xpath.startsWith("/"))xpath= TEMP_XMLN_TAG_PREFIX +xpath;
        return xpath.replaceAll("(?<=/)[A-z1-9]+", TEMP_XMLN_TAG_PREFIX +"$0");
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

    private static void synchronizeNameSpace(DefaultElement clone, Document readPomDocument) {
        if(readPomDocument.getRootElement().getNamespace()==null)return;
        clone.setNamespace(readPomDocument.getRootElement().getNamespace());
        for (Element elementChild : clone.elements()) {
            DefaultElement elementDefault = (DefaultElement)elementChild;
            synchronizeNameSpace(elementDefault, readPomDocument);
        }
    }

    private static Node selectSingleNodeListWithNSXpath(@Nullable HashMap<String, String> pomNameSpaceMap, Node readPomDocument, String replaceNodeXpath) {
        if(pomNameSpaceMap==null)return readPomDocument.selectSingleNode(replaceNodeXpath);
        XPath replaceNodeXpathSecond = readPomDocument.createXPath(xpathNS(replaceNodeXpath));
        replaceNodeXpathSecond.setNamespaceURIs(pomNameSpaceMap);
        return replaceNodeXpathSecond.selectSingleNode(readPomDocument);
    }

    private static List<Node> selectNodeListWithNSXpath(@Nullable HashMap<String, String> pomNameSpaceMap, Node readPomDocument, String replaceNodeXpath) {
        if(pomNameSpaceMap==null)return readPomDocument.selectNodes(replaceNodeXpath);
        XPath replaceNodeXpathSecond = readPomDocument.createXPath(xpathNS(replaceNodeXpath));
        replaceNodeXpathSecond.setNamespaceURIs(pomNameSpaceMap);
        return replaceNodeXpathSecond.selectNodes(readPomDocument);
    }


    private static Optional<HashMap<String, String>> createNamespaceMap(Document document) {
        HashMap<String,String> pomNameSpaceMap=new HashMap();
        String namespaceURI = document.getRootElement().getNamespaceURI();
        if(namespaceURI==null)return Optional.empty();
        pomNameSpaceMap.put(TEMP_XMLN_PREFIX,namespaceURI);
        return Optional.of(pomNameSpaceMap);
    }


}
