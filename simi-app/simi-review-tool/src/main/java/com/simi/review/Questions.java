package com.simi.review;

import com.simi.common.util.file.SimiFileUtils;
import lombok.Cleanup;
import lombok.extern.slf4j.Slf4j;

import java.io.*;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A utility class that reads a random line from {@code storage.txt}, and logs
 * it to {@code review-log.txt}.
 *
 * <p>
 * The priority value is a positive double, where zero means the line is ignored, and values
 * between 0 and 1 reduce the probability of being selected.
 * </p>
 *
 * @author Craig Brown
 * @since 1.1.0
 */
@Slf4j
public class Questions {
    private static final String STORAGE_FILE_PATH="C:\\Users\\simi\\Desktop\\DevProjects\\simi\\docs\\review-plan.txt";
    private static final String REVIEW_LOG_FILE_PATH = "C:\\Users\\simi\\Desktop\\DevProjects\\simi\\docs\\review-logs.txt";
    private static final DateTimeFormatter dateFormater= DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
    private static final Pattern PRIORITY_PATTERN = Pattern.compile("\\[(\\d+)]$");

    public static void main(String[] args) throws IOException {
        @Cleanup FileWriter fw = new FileWriter(REVIEW_LOG_FILE_PATH,true);
        // Algorithm Problems
        selectAndWriteARandomLineStartingWith(fw, "Algorithm Problems");
        // Questions
        // English
        // Japanese
    }

    private static void selectAndWriteARandomLineStartingWith(FileWriter fw, String... prefixes) throws IOException {
        String output = generateARandomLineStartingWith(prefixes);
        log.info("output: {}",output);
        fw.write(LocalDateTime.now().format(dateFormater) + " - "+ output+System.lineSeparator());
    }

    private static String generateARandomLineStartingWith(String ... prefixes) throws IOException {
        return SimiFileUtils.readRandomWeightedLine(
                Path.of(STORAGE_FILE_PATH),
                line -> !line.isEmpty() && Arrays.stream(prefixes).anyMatch(line::startsWith),
                line-> {
                    Matcher matcher=PRIORITY_PATTERN.matcher(line);
                    if(matcher.find()){
                        return Float.parseFloat(matcher.group(1));
                    }
                    return 1.0F;
                }
        );
    }
}
