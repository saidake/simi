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
        int randomInd = random.nextInt(strings.size());
        String output = strings.get(randomInd);
        List<String> associatedHeadings=new LinkedList<>();
        // [ 238 ] ProjectDesign = Design Patterns【设计师】[4] -> 骨人（拿起）[7] -> 香囊
        System.out.println(output);
        String prefix=output.replaceAll("【.*$","").replaceAll("\\[\\s[0-9]+\\s\\]","");
        System.out.println(prefix);
        System.out.println("=============================");
        for (String string : strings) {
            if(string.contains(prefix)){
                associatedHeadings.add(string);
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
