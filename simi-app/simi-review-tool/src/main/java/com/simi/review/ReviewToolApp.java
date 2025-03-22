package com.simi.review;

import cn.hutool.core.lang.Assert;
import com.simi.common.util.file.SimiFileUtils;
import lombok.Cleanup;
import lombok.extern.slf4j.Slf4j;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
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
    private static final Pattern FILE_PATH_PATTERN =Pattern.compile("^FILE: '(.*?)'$");
    private static final Pattern MATCH_PATTERN =Pattern.compile("^MATCH_PATTERN: '(.*?)'$");
    private static final Pattern REMOVAL_PATTERN =Pattern.compile("^REMOVAL_PATTERN: '(.*?)'$");

    private static final Path REVIEW_LOG_FILE_PATH =Path.of("C:\\Users\\simi\\Desktop\\DevProjects\\simi-docs\\temp\\ReviewLogs.md");


    private static final DateTimeFormatter dateFormater= DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
    private static final Pattern PRIORITY_PATTERN = Pattern.compile("\\[(\\d+)]$");

    public static void main(String[] args) throws IOException {
        System.out.println("\\*");
        @Cleanup FileWriter fw = new FileWriter(REVIEW_LOG_FILE_PATH.toString(),true);
        readMatchPatterns().forEach(System.out::println);
        // Select a random line
        //writeReviewLog(fw, japWord);
        //fw.write(System.lineSeparator());
    }

    private static Stack<String> readMatchPatterns() throws IOException {
        Stack<String> res=new Stack<>();
        // Open the Key Points file
        @Cleanup BufferedReader reader = new BufferedReader(new FileReader(KEY_POINTS.toString()));
        // Read random lines from external files
        String line;
        Path filePath=null;
        int lineNum=0;
        while ((line = reader.readLine()) != null) {
            lineNum++;
            if(line.isEmpty())continue;
            Matcher filePathMatcher=FILE_PATH_PATTERN.matcher(line);
            Matcher matchMatcher=MATCH_PATTERN.matcher(line);
            Matcher removalMatcher=REMOVAL_PATTERN.matcher(line);
            if(filePathMatcher.find()){
                // Add a file name prefix for the previous matched line
                if(!res.isEmpty()){
                    String curHandledStr=res.pop();
                    res.push(getFileNamePrefix(filePath.getFileName().toString())+curHandledStr);
                }
                String filePathStr=filePathMatcher.group(1);
                filePath=Path.of(filePathStr);
                log.info("--------------------------------------");
                log.info("filePath: {}",filePath);

            } else if(filePath!=null&&matchMatcher.find()){
                String currentMatchPattern=matchMatcher.group(1);
                log.info("currentMatchPattern: {}", currentMatchPattern);
                String currentRandomLine=SimiFileUtils.readRandomLine(filePath,item->item.matches(currentMatchPattern));
                log.info("currentRandomLine: {}", currentRandomLine);
                res.push(currentRandomLine);
            } else if(filePath!=null&&removalMatcher.find()){
                String currentRemovalPattern=removalMatcher.group(1);
                log.info("currentRemovalPattern: '{}'", currentRemovalPattern);
                log.info("res.peek(): {}", res.peek());
                Assert.notNull(res.peek());
                String temp=res.pop().replaceAll(currentRemovalPattern,"");
                log.info("temp: {}", temp);
                res.push(temp);
            }else{
                if(lineNum>0)lineNum--;
                // Add a file name prefix for the previous matched line
                if(!res.isEmpty()){
                    String curHandledStr=res.pop();
                    res.push(getFileNamePrefix(filePath.getFileName().toString())+curHandledStr);
                }
                break;
            }
        }
        // Read a random line form the Key Point file
        log.info("lineNum: {}",lineNum);
        List<String> collect = Files.lines(KEY_POINTS).skip(lineNum).filter(keyPointLine->!keyPointLine.isEmpty()).toList();
        Assert.notEmpty(collect);
        String curLine=collect.get(ThreadLocalRandom.current().nextInt(collect.size()));
        res.push(getFileNamePrefix(KEY_POINTS.getFileName().toString())+curLine);
        return res;
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
     * Write the output string into the {@code REVIEW_LOG_FILE_PATH} file.
     *
     * @param fw    File Writer
     * @param output    Output String
     * @throws IOException IO Exception
     */
    private static void writeReviewLog(FileWriter fw, String output) throws IOException {
        // fw.write(LocalDateTime.now().format(dateFormater) + " - "+ output);
        fw.write(output);
        fw.write(System.lineSeparator());
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
