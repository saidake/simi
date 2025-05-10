package com.simi.common.util.file;

import cn.hutool.core.lang.Assert;
import com.simi.common.util.SmpAssert;
import jakarta.annotation.Nullable;
import lombok.Cleanup;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.ThreadLocalRandom;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.UnaryOperator;
import java.util.stream.Stream;

/**
 * The class consists exclusively of static method for reading file or write file.
 *
 * @author Craig Brown
 * @since 1.0
 */
@Slf4j
@UtilityClass
public class SimiFileUtils {

    /**
     * Read a random line matching the predicate function from the specified file.
     *
     * @param path  Source file path
     * @param linePredicate Line filter
     * @return  A random line
     * @throws IOException IO Exception during the file reading process
     */
    public static String readRandomLine(Path path, @Nullable Predicate<String> linePredicate) throws IOException {
        @Cleanup Stream<String> lines = Files.lines(path);
        List<String> collect =
                linePredicate != null
                        ? lines.filter(linePredicate).toList()
                        : lines.toList();
        Assert.notEmpty(collect);
        return collect.get(ThreadLocalRandom.current().nextInt(collect.size()));
    }

    /**
     * Read a random line from the specified file.
     *
     * @param path  Source file path
     * @return  A random line
     * @throws IOException IO Exception during the file reading process
     */
    public static String readRandomLine(Path path) throws IOException {
        return readRandomLine(path, null);
    }

    /**
     * Read a random line matching the predicate function from the specified file based on the line priority.
     *
     * @param path Source file path
     * @param linePredicate Line filter
     * @return A random line
     * @throws IOException IO Exception during the file reading process
     */
    public static String readRandomWeightedLine(Path path,  Predicate<String> linePredicate, Function<String, Float> priorityMatcher) throws IOException {
        List<String> lines = Files.lines(path).filter(linePredicate).toList();
        List<Float> weights = lines.stream().map(priorityMatcher).toList();

        // Calculate the total weight
        double totalWeight = 0.0d;
        for(float weight: weights){
            totalWeight+=weight;
        }

        // Generate a random number based on the total weight
        double randomValue = Math.random() * totalWeight;

        // Select the line based on the weighted random value
        double cumulativeWeight = 0.0d;
        for (int i = 0; i < lines.size(); i++) {
            cumulativeWeight += weights.get(i);
            if (cumulativeWeight >= randomValue ) {
                return lines.get(i);
            }
        }

        // Return the last line in case of rounding issues
        return lines.get(lines.size() - 1);
    }

    /**
     * Reads a source file and write the file content as a parameter to the
     * target file.
     *
     * @param readPath the path of read file。(readPath can be exactly the same as writePath)
     * @param writePath the path of write file
     * @param lambda how to handle read strings
     * @throws IOException if there is an error reading the file.
     */
    public static void readAndWriteStringFile(String readPath, String writePath, UnaryOperator<String> lambda) throws IOException {
        SmpAssert.notNull(writePath,"writePath must not be null");
        SmpAssert.notNull(lambda,"lambda must not be null");
        String readString = Files.readString(Paths.get(readPath));
        Files.writeString(Paths.get(writePath),lambda.apply(readString), StandardCharsets.UTF_8);
    }

    /**
     * The append file content will be merged into the write file.
     *
     * @param readPropertiesPath the path of read file。(readPropertiesPath can be exactly the same as writePropertiesPath)
     * @param writePropertiesPath the path of write file。
     * @param lambda lambda
     * @param appendPath the path of read file
     * @throws IOException if there is an error reading the file.
     */
    public static void readAndPutAllProperties(@Nullable String readPropertiesPath, String writePropertiesPath, @Nullable UnaryOperator<Properties> lambda, String... appendPath) throws IOException {
        SmpAssert.notNull(appendPath,"readPath must not be null");
        SmpAssert.notNull(writePropertiesPath,"writePath must not be null");
        Properties writeProperties=new Properties();
        @Cleanup FileInputStream fileInputStream = new FileInputStream(readPropertiesPath);
        writeProperties.load(fileInputStream);
        Set<String> fileNameList=new HashSet<>();
        for (String appendPathItem : appendPath) {
            Path readPath = Paths.get(appendPathItem);
            fileNameList.add(readPath.getFileName().toString());
            Properties appendProperties=new Properties();
            appendProperties.load(new FileInputStream(appendPathItem));
            if(lambda!=null)appendProperties=lambda.apply(appendProperties);
            writeProperties.putAll(appendProperties);
        }
        @Cleanup FileOutputStream fileOutputStream = new FileOutputStream(writePropertiesPath);
        writeProperties.store(fileOutputStream,"merge the content of "+StringUtils.join(fileNameList,", ")+" file");
    }

    /**
     * The append file content will be merged into the write file.
     *
     * @param readPropertiesPath the path of read file。(readPropertiesPath can be exactly the same as writePropertiesPath)
     * @param writePropertiesPath the path of write file。
     * @param parentPath the path of read file
     * @throws IOException if there is an error reading the file.
     */
    public static void readAndPutAllPropertiesFromParent(@Nullable String readPropertiesPath, String writePropertiesPath, String parentPath, @Nullable UnaryOperator<Properties> lambda) throws IOException {
        SmpAssert.notNull(parentPath,"parentPath must not be null");
        SmpAssert.notNull(writePropertiesPath,"writePath must not be null");
        Properties writeProperties=new Properties();
        writeProperties.load(new FileInputStream(readPropertiesPath));
        Set<String> fileNameList=new HashSet<>();
        File parentFile = new File(parentPath);
        SmpAssert.isTrue(parentFile.exists(),"parentPath file doesn't exist: "+parentPath);
        if(parentFile.listFiles()==null)return;
        for (File childFile : parentFile.listFiles()) {
            Path readPath = Paths.get(childFile.getPath());
            fileNameList.add(readPath.getFileName().toString());
            Properties appendProperties=new Properties();
            appendProperties.load(new FileInputStream(childFile.getPath()));
            if(lambda!=null)appendProperties=lambda.apply(appendProperties);
            writeProperties.putAll(appendProperties);
        }
        writeProperties.store(new FileOutputStream(writePropertiesPath),"merge the content of "+StringUtils.join(fileNameList,", ")+" file");
    }

    /**
     * concat path and package name.
     *
     * @param path  source path
     * @param packageName   package name
     * @return  result path
     */
    public static String joinPathAndPackage(String path,String... packageName){
        for (String name : packageName) {
            name=name.replace(".",File.separator);
            path=path+File.separator;
            path=path.concat(name);
        }
        return path;
    }

    private static final String SDK_MARK_TAG="SDK_MARK_TAG";
    private static final String SDK_RETURN_MARK_TAG="SDK_RETURN_MARK_TAG";
    private static final ThreadLocal<Boolean> alreadyMarked=new ThreadLocal<>();

    public static void readWriteBackupFile(String readOrWritePath, @Nullable String writePath, @Nullable String appendContent, Function<String,String> lambda) throws IOException {
        SmpAssert.notNull(readOrWritePath,"readPath must not be null");
        SmpAssert.notNull(lambda,"lambda must not be null");
        File readFile = new File(readOrWritePath);
        File writeFile;
        if(writePath==null){
            File readTempFile = File.createTempFile(readFile.getName(), ".backup");
            org.apache.commons.io.FileUtils.copyFile(readFile,readTempFile);
            readFile=readTempFile;
            writeFile=new File(readOrWritePath);
        }else{
            writeFile = new File(writePath);
        }
        //A. common data
        boolean isSameFile= readOrWritePath.equals(writePath);
        //A. create temporary file.
        if(isSameFile){
            readFile = File.createTempFile(readFile.getName(), ".backup");
            org.apache.commons.io.FileUtils.copyFile(writeFile,readFile);
        }
        //A. execute lambda
        try {
            BufferedReader bufferedReader = new BufferedReader(new FileReader(readFile));
            BufferedWriter bufferedWriter=new BufferedWriter(new FileWriter(writeFile));
            for ( String currentLine = bufferedReader.readLine();currentLine!=null;currentLine = bufferedReader.readLine()){
                //B. execute lambda
                String resultLine = lambda.apply(currentLine);
                if(resultLine==null)continue;
                if(SDK_MARK_TAG.equals(resultLine)){
                    bufferedReader.mark((int)readFile.length()+1);
                    alreadyMarked.set(true);
                }else if(resultLine.startsWith(SDK_RETURN_MARK_TAG)){
                    bufferedReader.reset();
                    alreadyMarked.set(false);
                    bufferedWriter.write(resultLine.substring(SDK_RETURN_MARK_TAG.length()));
                }else if(alreadyMarked.get()==null||Boolean.FALSE.equals(alreadyMarked.get())){
                    bufferedWriter.write(resultLine);
                }
            }
            if(StringUtils.isNotBlank(appendContent))bufferedWriter.write(appendContent);
            if(Boolean.TRUE.equals(alreadyMarked.get())){
                alreadyMarked.set(false);
            }
            bufferedReader.close();
            bufferedWriter.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        //A. delete temporary file.
        if(isSameFile&&!readFile.delete()) {
            throw new RuntimeException("delete file error: "+readFile.getPath());
        }
    }


}
