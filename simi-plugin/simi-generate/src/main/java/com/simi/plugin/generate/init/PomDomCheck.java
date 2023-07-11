//package com.simi.plugin.generate.init;
//
//import com.intellij.util.xml.DomElement;
//import com.intellij.util.xml.DomFileElement;
//import com.intellij.util.xml.highlighting.DomElementAnnotationHolder;
//import com.intellij.util.xml.highlighting.DomElementsInspection;
//import org.jetbrains.annotations.NotNull;
//
//public class PomDomCheck extends DomElementsInspection {
//
//    public PomDomCheck(Class domClass, @NotNull Class[] additionalClasses) {
//        super(domClass, additionalClasses);
//    }
//
//    @Override
//    public void checkFileElement(DomFileElement domFileElement, final DomElementAnnotationHolder holder){
//        System.out.println("checkFileElement");
//        DomElement rootElement = domFileElement.getRootElement();
//        System.out.println(rootElement.getXmlElementName());
//        System.out.println(rootElement.getXmlElement().getText());
//    }
//}
