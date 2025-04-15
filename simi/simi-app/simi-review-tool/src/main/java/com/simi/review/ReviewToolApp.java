package com.simi.review;

import cn.hutool.core.lang.Assert;
import com.simi.common.util.file.SimiFileUtils;
import lombok.Cleanup;
import lombok.extern.slf4j.Slf4j;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.ThreadLocalRandom;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A utility class that reads a random line from several external files, and logs
 * it to the {@code REVIEW_LOG_FILE_PATH} file.
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
    private static final Path KEY_POINTS =Path.of("C:\\Users\\simi\\Desktop\\DevProjects\\simi-docs\\temp\\KeyPoints.md");
    private static final Pattern FILE_PATH_PATTERN =Pattern.compile("^FILE: '(.*?)'\\s*$");
    private static final Pattern MATCH_PATTERN =Pattern.compile("^MATCH_PATTERN: '(.*?)'\\s*$");
    private static final Pattern REMOVAL_PATTERN =Pattern.compile("^REMOVAL_PATTERN: '(.*?)'\\s*$");
    private static final Pattern NUMBER_PATTERN =Pattern.compile("^NUMBER: ([1-9]+)\\s*$");

    private static final Path REVIEW_LOG_FILE_PATH =Path.of("C:\\Users\\simi\\Desktop\\DevProjects\\simi-docs\\temp\\ReviewLogs.md");

    private static final DateTimeFormatter dateFormater= DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

    public static void main(String[] args) throws IOException {
        System.out.println("\\*");
        Stack<String> lines= generateRandomLines();
        System.out.println("-------------------------------------------");
        lines.forEach(System.out::println);
        prependLines(lines);
    }

    /**
     * Generate random lines based on the patterns and content in the KeyPoints file.
     *
     * @return Random lines
     * @throws IOException IO Exception
     */
    private static Stack<String> generateRandomLines() throws IOException {
        Stack<String> res=new Stack<>();
        // Open the Key Points file
        @Cleanup BufferedReader reader = new BufferedReader(new FileReader(KEY_POINTS.toString()));
        // Read random lines from external files
        String line;
        int skipLineNum=0;

        Path filePath=null;
        int randomLineNum=1;
        String currentMatchPattern=null;
        String currentRemovalPattern=null;
        while ((line = reader.readLine()) != null) {
            skipLineNum++;
            if(line.isEmpty())continue;
            Matcher filePathMatcher=FILE_PATH_PATTERN.matcher(line);
            Matcher matchMatcher=MATCH_PATTERN.matcher(line);
            Matcher removalMatcher=REMOVAL_PATTERN.matcher(line);
            Matcher numberMatcher=NUMBER_PATTERN.matcher(line);
            if(filePathMatcher.find()){
                // Add a file name prefix for the previous matched line
                if(filePath!=null){
                    Assert.notNull(currentMatchPattern);
                    pushRandomLine(randomLineNum, currentMatchPattern, filePath, currentRemovalPattern, res);
                    currentMatchPattern=null;
                    currentRemovalPattern=null;
                    randomLineNum=1;
                }
                String filePathStr=filePathMatcher.group(1);
                filePath=Path.of(filePathStr);
            } else if(filePath!=null&&matchMatcher.find()){
                currentMatchPattern=matchMatcher.group(1);
            } else if(filePath!=null&&removalMatcher.find()) {
                currentRemovalPattern = removalMatcher.group(1);
            } else if (filePath!=null&&numberMatcher.find()){
                String currentNumber=numberMatcher.group(1);
                Assert.notNull(res.peek());
                randomLineNum=Integer.valueOf(currentNumber);
            }else{
                if(skipLineNum>0)skipLineNum--;
                // Add a file name prefix for the previous matched line
                if(filePath!=null){
                    Assert.notNull(currentMatchPattern);
                    pushRandomLine(randomLineNum, currentMatchPattern, filePath, currentRemovalPattern, res);
                }
                break;
            }
        }
        // Read a random line form the Key Point file
        log.info("lineNum: {}",skipLineNum);
        List<String> collect = Files.lines(KEY_POINTS).skip(skipLineNum).filter(keyPointLine->!keyPointLine.isEmpty()).toList();
        Assert.notEmpty(collect);
        String curLine=collect.get(ThreadLocalRandom.current().nextInt(collect.size()));
        res.push(getFileNamePrefix(KEY_POINTS.getFileName().toString())+curLine);
        return res;
    }

    /**
     * Push a random line to the stack result.
     *
     * @param randomLineNum The number of random lines to be generated
     * @param currentMatchPattern The current match pattern
     * @param filePath  The external file path
     * @param currentRemovalPattern The removal match pattern
     * @param res The stack result including random lines
     * @throws IOException IO exception
     */
    private static void pushRandomLine(int randomLineNum, String currentMatchPattern, Path filePath, String currentRemovalPattern, Stack<String> res) throws IOException {
        for(; randomLineNum >0; randomLineNum--){
            String finalCurrentMatchPattern = currentMatchPattern;
            String result=SimiFileUtils.readRandomLine(filePath, item->item.matches(finalCurrentMatchPattern));
            if(currentRemovalPattern!=null) result = result.replaceAll(currentRemovalPattern, "");
            log.info("result: {}", result);
            result=getFileNamePrefix(filePath.getFileName().toString())+result;
            res.push(result);
        }
    }

    /**
     * Return a handled file name prefix.
     *
     * @param fileName A file name
     * @return  The new file name
     */
    private static String getFileNamePrefix(String fileName){
        return fileName.replaceAll(".md","")+": ";
    }


    /**
     * Prepend the random lines to the review log file.
     *
     * @param newLines  Final random lines
     * @throws IOException  IO exception
     */
    public static void prependLines(Stack<String> newLines) throws IOException {
        Path tempPath = Files.createTempFile("tempFile", ".tmp");

        try (BufferedWriter writer = Files.newBufferedWriter(tempPath);
             BufferedReader reader = Files.newBufferedReader(REVIEW_LOG_FILE_PATH)) {

            // Write new lines at the top
            for (String line : newLines) {
                writer.write(line);
                writer.newLine();
            }

            // Append original content
            String existingLine;
            while ((existingLine = reader.readLine()) != null) {
                writer.write(existingLine);
                writer.newLine();
            }
        }

        // Replace original file with the temp file
        Files.move(tempPath, REVIEW_LOG_FILE_PATH, StandardCopyOption.REPLACE_EXISTING);
    }
}
