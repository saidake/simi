package com.simi.memory;

import org.apache.commons.lang3.StringUtils;
import org.apache.poi.xwpf.usermodel.XWPFDocument;
import org.apache.poi.xwpf.usermodel.XWPFParagraph;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.*;

public class Initialization {
    public static final String SEPARATOR=" = ";
    public static void main(String[] args) throws IOException {
        FileInputStream fis = new FileInputStream(new File("C:\\Users\\simi\\Desktop\\OneDrive\\AAAMemory\\AAD--Memory.docx"));
        FileOutputStream fos = new FileOutputStream("C:\\Users\\simi\\Desktop\\DevProject\\simi\\simi-app\\simi-memory-management\\src\\main\\resources\\line-numbers.txt");
        XWPFDocument document = new XWPFDocument(fis);
        List<String> resultList = new ArrayList<>();
        Deque<String> titleList = new LinkedList<>();
        Deque<Integer> levelList = new LinkedList<>();
        // 读取文档的标题
//        int lineNumber = 0;
        for (XWPFParagraph para : document.getParagraphs()) {
            if (para.getStyle() != null && para.getStyle().startsWith("Heading")) {
//                lineNumber++;
                int level = Integer.parseInt(para.getStyle().substring("Heading".length()));
                if (levelList.isEmpty() || level > levelList.peekLast()) {
                    levelList.add(level);
                    titleList.add(para.getText());
                } else if (level < levelList.peekLast()) {
                    while (levelList.peekLast() != null && level <= levelList.peekLast()) {
                        levelList.pollLast();
                        titleList.pollLast();
                    }
                    levelList.add(level);
                    titleList.add(para.getText());
                } else {
                    titleList.pollLast();
                    titleList.add(para.getText());
                }
                //A. return results
                if (level >2 ) {
//                    StringBuilder stringBuilder = new StringBuilder("[ " + lineNumber + " ] ");
                    StringBuilder stringBuilder = new StringBuilder();
                    String resSeparator = " "+SEPARATOR+" ";
                    for (String next : titleList) {
                        stringBuilder.append(next);
                        stringBuilder.append(resSeparator);
                    }
                    String str = stringBuilder.toString();
                    resultList.add(str.substring(0, str.length() - resSeparator.length() + 1));
                }
            } else if(para.getStyle()==null){
                resultList.add(para.getText());
            }
        }
        String join = StringUtils.join(resultList, System.lineSeparator());
        fos.write(join.getBytes());
        fis.close();
    }
}
