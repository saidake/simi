package com.simi.sandbox.libtest;

import lombok.Cleanup;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;

public class IOTest {
    public static void main(String[] args) throws IOException {
        File file = new File("C:\\Users\\simi\\Desktop\\DevProjects\\simi\\simi-sandbox\\simi-aaa\\simi-lib-test\\src\\main\\resources\\test.csv");
        String message = "Exception occurred, ignore message";

        @Cleanup PrintWriter writer = new PrintWriter(new FileWriter(file));
        // Write the header
        writer.println("Column1,Column2");
        // Handle message with commas
        writer.println("Row1," + message.replaceAll(",",";"));
    }
}
