package com.simi.memory;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Random;

public class RandomQuestions {
    public static void main(String[] args) throws IOException {
        Path path = Path.of("C:\\Users\\simi\\Desktop\\DevProject\\simi\\simi-app\\simi-memory-management\\src\\main\\resources\\storage.txt");
        List<String> strings = Files.readAllLines(path);
        Random randomEx = new Random();
        Random random = new Random();
        //A. get valid line
        int randomInd = random.nextInt(strings.size());
        int randomExInd=randomEx.nextInt(4);
        String output = strings.get(randomInd);
        System.out.println(output);
        //A. get prefix (e.g. ProjectDesign = Design Patterns【... -> ProjectDesign = Design Patterns ).
        System.out.println("=============================");
        switch (randomExInd){
            case 0:
                System.out.println("External Questions");
            default:
                System.out.println("pdf: "+(random.nextInt(54)+1));
        }
        System.out.println("=============================");
        FileWriter fw = new FileWriter("C:\\Users\\simi\\Desktop\\DevProject\\simi\\simi-app\\simi-memory-management\\src\\main\\resources\\memory-log.txt",true);
        fw.write(output+System.lineSeparator());
        fw.close();
    }
}
