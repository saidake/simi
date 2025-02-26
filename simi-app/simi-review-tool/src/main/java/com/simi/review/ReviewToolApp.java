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
 * The priority value is a positive double, where zero means the line is ignored,
 * and values between 0 and 1 reduce the probability of being selected.
 * </p>
 *
 * @author Craig Brown
 * @since 1.1.0
 */
@Slf4j
public class ReviewToolApp {
    private static final Path DOC_KEY_POINTS_PATH =Path.of("C:\\Users\\simi\\Desktop\\DevProjects\\simi\\docs\\doc-key-points.txt");
    private static final Path ALGORITHMS_PATH =Path.of("C:\\Users\\simi\\Desktop\\DevProjects\\simi\\docs\\Algorithms.md");
    private static final Path REVIEW_LOG_FILE_PATH = Path.of("C:\\Users\\simi\\Desktop\\DevProjects\\simi\\docs\\review-logs.txt");
    private static final DateTimeFormatter dateFormater= DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
    private static final Pattern PRIORITY_PATTERN = Pattern.compile("\\[(\\d+)]$");

    public static void main(String[] args) throws IOException {
        @Cleanup FileWriter fw = new FileWriter(REVIEW_LOG_FILE_PATH.toString(),true);
        // Algorithms
        String algorithmHeading=SimiFileUtils.readRandomLine(ALGORITHMS_PATH, line->line.matches("^## [1-9]+.*?")).replaceAll("^## ","");
        log.info("algorithmHeading: {}", algorithmHeading);
        // Questions
        // English
        // Japanese
        writeReviewLog(fw, algorithmHeading);
        fw.write(System.lineSeparator());
        fw.write(System.lineSeparator());
    }

    /**
     * Write the output string into the {@code REVIEW_LOG_FILE_PATH} file.
     *
     * @param fw    File Writer
     * @param output    Output String
     * @throws IOException IO Exception
     */
    private static void writeReviewLog(FileWriter fw, String output) throws IOException {
        fw.write(LocalDateTime.now().format(dateFormater) + " - "+ output);
    }

    /**
     * Filter out a line starting with specific prefixes.
     *
     * @param prefixes Prefixes
     * @return A line
     * @throws IOException IO exception
     */
    private static String generateARandomLineStartingWith(Path path,String ... prefixes) throws IOException {
        return SimiFileUtils.readRandomWeightedLine(
                path,
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


    /**
     * Filter out a random line from the {@code REVIEW_PLAN_PATH} path.
     *
     * @return A line
     * @throws IOException IO exception
     */
    private static String generateARandomLine(Path path) throws IOException {
        return SimiFileUtils.readRandomWeightedLine(
                path,
                line -> !line.isEmpty(),
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
