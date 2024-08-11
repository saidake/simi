package com.simi.memory;

import org.apache.commons.lang3.StringUtils;
import org.apache.poi.xwpf.usermodel.XWPFDocument;
import org.apache.poi.xwpf.usermodel.XWPFHeader;
import org.apache.poi.xwpf.usermodel.XWPFParagraph;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

public class Questions {
    public static void main(String[] args) throws IOException {
        Path path = Path.of("C:\\Users\\simi\\Desktop\\DevProject\\simi\\simi-app\\simi-memory-management\\src\\main\\resources\\line-numbers.txt");
        List<String> strings = Files.readAllLines(path);
        Random random = new Random();
        //A. get valid line
        boolean invalidOutput=true;
        String output=null;
        while(invalidOutput){
            int randomInd = random.nextInt(strings.size());
            output = strings.get(randomInd);
            if(output.contains(Initialization.SEPARATOR)){
                invalidOutput=false;
                // System.out.println("index: "+randomInd);
            }
        }
        List<String> associatedHeadings=new LinkedList<>();
        // [ 238 ] ProjectDesign = Design Patterns【设计师】[4] -> 骨人（拿起）[7] -> 香囊
        System.out.println(output);
        //A. get prefix (e.g. ProjectDesign = Design Patterns【... -> ProjectDesign = Design Patterns ).
        String prefix=output.replaceAll("【.*$","").replaceAll("\\[\\s[0-9]+\\s\\]","");
        System.out.println("=============================");
        System.out.println("pdf: "+(random.nextInt(54)+1));
        System.out.println("=============================");
        boolean startCheckingText=false;
        for (String string : strings) {
            if(!string.contains(Initialization.SEPARATOR)&&startCheckingText){
                associatedHeadings.add(string);
                continue;
            }
            if(string.contains(prefix)){
                startCheckingText=true;
                associatedHeadings.add(string);
            }else{
                startCheckingText=false;
            }

        }
        for (String associatedHeading : associatedHeadings) {
            System.out.println(associatedHeading);
        }
        FileWriter fw = new FileWriter("C:\\Users\\simi\\Desktop\\DevProject\\simi\\simi-app\\simi-memory-management\\src\\main\\resources\\memory-log.txt",true);
        fw.write(output+System.lineSeparator());
        fw.close();
    }
}
