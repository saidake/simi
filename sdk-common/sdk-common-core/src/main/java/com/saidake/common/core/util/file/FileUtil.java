package com.saidake.common.core.util.file;

import org.apache.commons.lang3.StringUtils;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Stream;

public class FileUtil {

    /**
     * 拼接路径和包名
     * @param path
     * @param packageName
     * @return
     */
    public static String joinPathAndPackage(String path,String packageName){
        packageName=packageName.replace(".",File.separator);
        path=path+File.separator;
        return path.concat(packageName);
    }

    /**
     * 读写文件，对每一行做操作
     */
    @FunctionalInterface
    public interface ReadAndWriteTheSameFileLambda<T,R>{
        R execute(String T);
    }
    public static void readAndWriteTheSameFile(String readPath,String writePath,ReadAndWriteTheSameFileLambda<String,String> readAndWriteTheSameFileLambda){
        File readFile = new File(readPath);
        File writeFile = new File(writePath);
        if(!readFile.exists()){
            throw new RuntimeException("file not exist");
        }
        List<String> storageReadLine=new ArrayList<>();
        try {
            BufferedReader bufferedReader = new BufferedReader(new FileReader(readFile));
            for ( String currentLine = bufferedReader.readLine();currentLine!=null;currentLine = bufferedReader.readLine()){
                storageReadLine.add(currentLine);
            }
            BufferedWriter bufferedWriter=new BufferedWriter(new FileWriter(writeFile));
            for (String currentStorageLine : storageReadLine) {
                // Do your code here
                String resultLine = readAndWriteTheSameFileLambda.execute(currentStorageLine);
                bufferedWriter.write(resultLine);
            }
            bufferedReader.close();
            bufferedWriter.close();
        } catch (IOException e) {
            e.printStackTrace();
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
     * 清空其他文件，除了文件名 在stringList里，有任何一个开头的
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
