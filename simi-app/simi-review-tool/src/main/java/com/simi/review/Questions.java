package com.simi.review;

import com.simi.common.util.file.SimiFileUtils;
import lombok.Cleanup;

import java.io.*;
import java.net.URISyntaxException;
import java.nio.file.Path;
import java.util.*;

/**
 * A utility class that reads a random line from {@code storage.txt}, and logs
 * it to {@code review-log.txt}.
 *
 * @author Craig Brown
 * @since 1.1.0
 */
public class Questions {
    private static final String STORAGE_FILE_PATH="C:\\Users\\simi\\Desktop\\DevProjects\\simi\\simi-app\\simi-review-tool\\src\\main\\resources\\storage.txt";
    private static final String REVIEW_LOG_FILE_PATH = "C:\\Users\\simi\\Desktop\\DevProjects\\simi\\simi-app\\simi-review-tool\\src\\main\\resources\\review-log.txt";
    private static final List<String> PREFIX_LIST=new ArrayList<>();
    static {
//        PREFIX_LIST.add("Config = Work Environment = awscli = Command");
    }
    public static void main(String[] args) throws IOException, URISyntaxException {
        //A. filter out valid line and obtain a random line.
        String output = SimiFileUtils.readRandomLine(Path.of(STORAGE_FILE_PATH), line -> PREFIX_LIST.isEmpty() || PREFIX_LIST.stream().anyMatch(line::startsWith));
        //A. filter out invalid line
        System.out.println(output);
        @Cleanup FileWriter fw = new FileWriter(REVIEW_LOG_FILE_PATH,true);
        fw.write(output+System.lineSeparator());
    }
}
