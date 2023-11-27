package com.simi;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class IOTest {
    public static void main(String[] args) throws IOException {
        Path path = Paths.get("D:\\Desktop\\DevProject\\simi\\sdk-service\\sdk-generator\\src\\test\\java\\com\\saidake\\dashboard.txt");
        System.out.println(path.toString());
        Path outPath = Paths.get("D:\\Desktop\\DevProject\\simi\\sdk-service\\sdk-generator\\src\\test\\java\\com\\saidake\\test2.txt");
        String readString = Files.readString(path);
        String ddd = readString.concat("ddd");
        Files.writeString(outPath,ddd, StandardCharsets.UTF_8);
    }
}
