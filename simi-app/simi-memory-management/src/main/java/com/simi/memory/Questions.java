package com.simi.memory;

import com.simi.common.util.file.SimiFileUtils;
import lombok.Cleanup;

import java.io.*;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

public class Questions {
    private static final String STORAGE_FILE_PATH="/storage.txt";
    private static final String MEMORY_LOG_FILE_PATH="C:\\Users\\simi\\Desktop\\DevProjects\\simi\\simi-app\\simi-memory-management\\src\\main\\resources\\memory-log.txt";
    private static final List<String> PREFIX_LIST=new ArrayList<>();
    static {
        PREFIX_LIST.add("Config = Work Environment = awscli = Command");
    }
    public static void main(String[] args) throws IOException, URISyntaxException {
        URI storageFilePath = Objects.requireNonNull(Questions.class.getResource(STORAGE_FILE_PATH)).toURI();
        //A. filter out valid line and obtain a random line.
        String output = SimiFileUtils.readRandomLine(Path.of(storageFilePath), line -> PREFIX_LIST.stream().anyMatch(line::startsWith));
        //A. filter out invalid line
        System.out.println(output);
        @Cleanup FileWriter fw = new FileWriter(MEMORY_LOG_FILE_PATH,true);
        fw.write(output+System.lineSeparator());
    }
}
