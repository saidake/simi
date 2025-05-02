package com.simi.review;

import com.simi.common.util.file.SimiFileUtils;
import com.simi.review.domain.ReviewTask;
import com.simi.review.domain.ReviewYmlProperties;
import lombok.Cleanup;
import lombok.extern.slf4j.Slf4j;
import org.yaml.snakeyaml.Yaml;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.format.DateTimeFormatter;
import java.util.*;

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
    private static final Path CONFIG_FILE =Path.of("C:\\Users\\simi\\Desktop\\DevProjects\\simi-docs\\review.yml");
    private static final Path KEY_POINTS =Path.of("C:\\Users\\simi\\Desktop\\DevProjects\\simi-docs\\temp\\KeyPoints.txt");
    private static final Path REVIEW_LOG_FILE_PATH =Path.of("C:\\Users\\simi\\Desktop\\DevProjects\\simi-docs\\temp\\ReviewLogs.md");

    private static final DateTimeFormatter dateFormater= DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

    public static void main(String[] args) throws IOException {
        // Load configuration file
        Yaml configYml=new Yaml();
        @Cleanup InputStream inputStream = new FileInputStream(CONFIG_FILE.toString());
        ReviewYmlProperties reviewYmlProperties = configYml.loadAs(inputStream, ReviewYmlProperties.class);
        // Read random lines
        List<String> lines=new LinkedList<>();
        for(ReviewTask task :reviewYmlProperties.getTasks()){
            Path filePath=Path.of(task.getFile());
            String result=task.getMatchPattern()==null?
                    SimiFileUtils.readRandomLine(filePath, item->!item.isBlank())
                    :SimiFileUtils.readRandomLine(filePath, item->item.matches(task.getMatchPattern()));
            if(task.getRemovalPattern()!=null) result = result.replaceAll(task.getRemovalPattern(), "");
            log.info("result: {}", result);
            result=filePath.getFileName().toString()+" : "+result;
            lines.add(result);
        }
        // Save results into review log file.
        System.out.println("-------------------------------------------");
        lines.forEach(System.out::println);
        prependLines(lines);
    }

    /**
     * Prepend the random lines to the review log file.
     *
     * @param newLines  Final random lines
     * @throws IOException  IO exception
     */
    public static void prependLines(List<String> newLines) throws IOException {
        Path tempPath = Files.createTempFile("tempFile", ".tmp");
        @Cleanup BufferedWriter writer = Files.newBufferedWriter(tempPath);
        @Cleanup BufferedReader reader = Files.newBufferedReader(REVIEW_LOG_FILE_PATH);

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
        // Replace original file with the temp file
        Files.move(tempPath, REVIEW_LOG_FILE_PATH, StandardCopyOption.REPLACE_EXISTING);
    }
}
