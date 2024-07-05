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
        System.out.println(output);
        FileWriter fw = new FileWriter("C:\\Users\\simi\\Desktop\\DevProject\\simi\\simi-app\\simi-memory-management\\src\\main\\resources\\memory-log.txt",true);
        fw.write(output+System.lineSeparator());
        fw.close();
    }
}
