package com.simi.memory;

import org.apache.poi.xwpf.usermodel.XWPFDocument;
import org.apache.poi.xwpf.usermodel.XWPFHeader;
import org.apache.poi.xwpf.usermodel.XWPFParagraph;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.*;

public class Questions {
    public static void main(String[] args) throws IOException {
        FileInputStream fis = new FileInputStream(new File("C:\\Users\\simi\\Desktop\\OneDrive\\AAAMemory\\AAD--Memory.docx"));
        XWPFDocument document = new XWPFDocument(fis);
        List<String> resultList=new ArrayList<>();
        Deque<String> titleList=new LinkedList<>();
        Deque<Integer> levelList=new LinkedList<>();
        // 读取文档的标题

        for (XWPFParagraph para : document.getParagraphs()) {
            if (para.getStyle() != null && para.getStyle().startsWith("Heading")) {
                // 获取标题级别
                int level = Integer.parseInt(para.getStyle().substring("Heading".length()));
                if(levelList.isEmpty() || level>levelList.peekLast()){
                    levelList.add(level);
                    titleList.add(para.getText());
                }else if(level<levelList.peekLast()){
                    while(levelList.peekLast()!=null&&level<=levelList.peekLast()){
                        levelList.pollLast();
                        titleList.pollLast();
                    }
                    levelList.add(level);
                    titleList.add(para.getText());
                }else{
                    titleList.pollLast();
                    titleList.add(para.getText());
                }
                if(level==8){
                    Iterator<String> iterator = titleList.iterator();
                    StringBuilder stringBuilder=new StringBuilder();
                    String Separator=" -> ";
                    while(iterator.hasNext()){
                        String next = iterator.next();
                        stringBuilder.append(next);
                        stringBuilder.append(Separator);
                    }
                    String str = stringBuilder.toString();
                    resultList.add(str.substring(0, str.length()-Separator.length()+1));
                }
            }
        }
        fis.close();
        Random random = new Random();
        int randomInd = random.nextInt(resultList.size());
        System.out.println(resultList.get(randomInd));
    }
}
