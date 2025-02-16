package com.simi.review;

import com.simi.common.util.file.SimiFileUtils;
import lombok.Cleanup;
import lombok.extern.slf4j.Slf4j;

import java.io.*;
import java.nio.file.Path;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A utility class that reads a random line from {@code storage.txt}, and logs
 * it to {@code review-log.txt}.
 *
 * @author Craig Brown
 * @since 1.1.0
 */
@Slf4j
public class Questions {
    private static final String STORAGE_FILE_PATH="C:\\Users\\simi\\Desktop\\DevProjects\\simi\\docs\\review-plans.txt";
    private static final String REVIEW_LOG_FILE_PATH = "C:\\Users\\simi\\Desktop\\DevProjects\\simi\\docs\\review-logs.txt";
    private static final List<String> PREFIX_LIST=new ArrayList<>();
    static {
        //PREFIX_LIST.add("Config = Work Environment = awscli = Command");
    }
    private static final Pattern PRIORITY_PATTERN = Pattern.compile("\\[(\\d+)]$");

    private static final Map<Integer, Double> REVIEW_PRIORITY_MAP=Map.of(
            1,1.0,
            2,1.0,
            3,1.0,
            4,1.0,
            5,1.0,
            6,2.0,
            7,4.0,
            8,6.0,
            9,8.0,
            10,10.0
    );

    public static void main(String[] args) throws IOException {
        // Filter out valid line and obtain a random line.
        String output=SimiFileUtils.readRandomWeightedLine(
                Path.of(STORAGE_FILE_PATH),
                REVIEW_PRIORITY_MAP,
                line -> !line.isEmpty()&&( PREFIX_LIST.isEmpty() || PREFIX_LIST.stream().anyMatch(line::startsWith)),
                line-> {
                    Matcher matcher=PRIORITY_PATTERN.matcher(line);
                    if(matcher.find()){
                        return Integer.parseInt(matcher.group(1));
                    }
                    return 5;
                }
        );
        // Filter out invalid line
        log.info("output: {}",output);
        @Cleanup FileWriter fw = new FileWriter(REVIEW_LOG_FILE_PATH,true);
        fw.write(output+System.lineSeparator());
    }
}
