package com.simi.memory;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Random;

public class Questions {
    public static void main(String[] args) throws IOException {
        List<String> stringList = Files.readAllLines(Path.of("C:\\Users\\simi\\Desktop\\DevProject\\simi\\simi-app\\simi-memory-management\\src\\main\\resources\\Java.txt"));
        Random random = new Random();
        String s;
        do{
            int i=random.nextInt(stringList.size());
            s= stringList.get(i);
        }while(!s.matches("^\\s*?[1-9]\\.\\s*?.*?"));
        System.out.println(s);
    }
}
