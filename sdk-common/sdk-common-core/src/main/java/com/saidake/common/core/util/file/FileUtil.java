package com.saidake.common.core.util.file;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Stream;

@Slf4j
public class FileUtil {
    public static String SDK_MARK_TAG="SDK_MARK_TAG";
    public static String SDK_RETURN_MARK_TAG="SDK_RETURN_MARK_TAG";
    private static ThreadLocal<Boolean> alreadyMarked=new ThreadLocal<>();

    /**
     * 拼接路径和包名
     * @param path  路径
     * @param packageName   包名
     * @return  拼接路径
     */
    public static String joinPathAndPackage(String path,String packageName){
        packageName=packageName.replace(".",File.separator);
        path=path+File.separator;
        return path.concat(packageName);
    }

    /**
     * 读写文件，对每一行做操作
     * 可选功能：标记一行，向下获取信息（不写入新行），再返回之前地那一行
     *         返回 "SDK_MARK_TAG"             标记行
     *         返回 "SDK_RETURN_MARK_TAG????"  返回标记行，后方拼接内容为标记行最终写入内容
     */
    @FunctionalInterface
    public interface ReadAndWriteTheSameFileLambda<T,R>{
        R execute(T t);
    }
    public static void readAndWriteFile(String readPath,String writePath,ReadAndWriteTheSameFileLambda<String,String> readAndWriteTheSameFileLambda) throws IOException {
        //A. 公共数据
        File readFile = new File(readPath);
        File writeFile = new File(writePath);
        boolean isSameFile= readPath.equals(writePath);
        //A. 创建临时文件
        if(isSameFile){
            readFile = File.createTempFile(readFile.getName(), ".backup");  // 临时读取文件
            FileUtils.copyFile(writeFile,readFile);
            log.info("create temp file successfully: {}",readFile.getPath());
        }
        //A. 执行匿名函数
        try {
            BufferedReader bufferedReader = new BufferedReader(new FileReader(readFile));
            BufferedWriter bufferedWriter=new BufferedWriter(new FileWriter(writeFile));
            for ( String currentLine = bufferedReader.readLine();currentLine!=null;currentLine = bufferedReader.readLine()){
                // Do your code here
                String resultLine = readAndWriteTheSameFileLambda.execute(currentLine);
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
            if(Boolean.TRUE.equals(alreadyMarked.get())){
                log.error("didn't return SDK_RETURN_MARK_TAG");
                alreadyMarked.set(false);
            }
            bufferedReader.close();
            bufferedWriter.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        //A. 删除临时文件
        if(isSameFile&&!readFile.delete()) {
           throw new RuntimeException("delete file error: "+readFile.getPath());
        }else{
            log.info("delete temp file successfully: {}",readFile.getPath());
        }
    }


    /**
     * 拼接多路径
     * @return
     */
    public static String joinPath(String... pathList){
        if (pathList.length==0){
            throw new RuntimeException("empty pathList");
        };
        if(pathList.length==1)return pathList[0];

        List<String> resultList=new ArrayList<>();
        resultList.add(pathList[0]);
        for (int i = 1; i < pathList.length; i++) {
            String currentItem=pathList[i];
            if(currentItem.length()==0)continue;
            String beforeItem=pathList[i-1];
            Boolean isBefore=beforeItem.endsWith(File.separator);
            Boolean isCurrent=currentItem.startsWith(File.separator);
            if(isBefore&&isCurrent){
                currentItem=currentItem.substring(1);
            }else if(!isBefore&&!isCurrent){
                currentItem=File.separator+currentItem;
            }
            resultList.add(currentItem);
        }
        return StringUtils.join(resultList.toArray());
    }

    /**
     * 清空其他文件，除了以 excludeFileOrFolder 开头的
     * @param sourcePath
     * @param excludeFileOrFolder
     */
    public static void clearOtherFilesStartsWith(String sourcePath,String excludeFileOrFolder){
        File source =new File(sourcePath);
        File[] listFiles = source.listFiles();
        assert listFiles != null;
        for( File currentFile:listFiles){
            if(currentFile.getName().startsWith(excludeFileOrFolder))continue;
            try {
                deleteFileOrDirectory(currentFile);
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    /**
     * Clear other files in sourcePath except fileName starts with stringList.
     *
     * @param sourcePath
     * @param stringList
     */
    public static void clearOtherFilesStartsWithByList(String sourcePath, List<String> stringList ){
        File source =new File(sourcePath);
        File[] listFiles = source.listFiles();
        if(listFiles==null||listFiles.length==0)return;
        for( File currentFile:listFiles){
            if(stringList.stream().anyMatch(item->currentFile.getName().startsWith(item))){
                continue;
            }
            try {
                deleteFileOrDirectory(currentFile);
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    private static void deleteFileOrDirectory(File file) throws IOException {
        if(file.isDirectory()){
            try (Stream<Path> walk = Files.walk(Paths.get(file.getPath()))) {
                walk.sorted(Comparator.reverseOrder())
                        .forEach(FileUtil::deleteDirectoryStream);
            }
        }else{
            deleteDirectoryStream(Paths.get(file.getPath()));
        }
    }

    private static void deleteDirectoryStream(Path path) {
        try {
            Files.delete(path);
            System.out.printf("删除文件成功：%s%n",path.toString());
        } catch (IOException e) {
            System.err.printf("无法删除的路径 %s%n%s", path, e);
        }
    }

}
