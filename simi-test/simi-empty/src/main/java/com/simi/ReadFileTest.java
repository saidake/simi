package com.simi;

/*
Given a file of size 1 gigabyte, each line in the file is a word, and each word is limited to 16 bytes.
Return the 100 most frequent words, with a memory limit of 10 megabytes.
 */

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Map;
import java.util.PriorityQueue;

public class ReadFileTest {
    public static void main(String[] args) {
        String filePath="ddd";
        int fileSize=1024*1024; // bytes
        int heapSize=100*1024; // 100M
        PriorityQueue<Map.Entry<String, Integer>> objects = new PriorityQueue<>();
        try (BufferedReader bufferedReader=new BufferedReader(new FileReader(filePath));){
            char[] buffer=new char[fileSize];
        } catch (FileNotFoundException e) {


        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
