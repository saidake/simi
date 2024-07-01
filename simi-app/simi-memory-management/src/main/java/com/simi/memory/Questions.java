package com.simi.memory;

import org.apache.poi.xwpf.usermodel.XWPFDocument;
import org.apache.poi.xwpf.usermodel.XWPFHeader;
import org.apache.poi.xwpf.usermodel.XWPFParagraph;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

public class Questions {
    public static void main(String[] args) throws IOException {
        FileInputStream fis = new FileInputStream(new File("C:\\Users\\simi\\Desktop\\OneDrive\\AAAMemory\\AAD--Memory.docx"));
        XWPFDocument document = new XWPFDocument(fis);

        // 读取文档的标题
        for (XWPFParagraph para : document.getParagraphs()) {
            if (para.getStyle() != null && para.getStyle().startsWith("Heading")) {
                // 获取标题级别
                int level = Integer.parseInt(para.getStyle().substring("Heading".length()));
                // 打印标题内容和级别
                System.out.println("Level " + level + ": " + para.getText());
            }
        }

        fis.close();
    }
}
