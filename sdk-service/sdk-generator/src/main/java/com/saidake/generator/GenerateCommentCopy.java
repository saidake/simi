//package com.saidake.generator;
//import com.saidake.common.core.util.file.FileUtil;
//import lombok.extern.slf4j.Slf4j;
//
//import java.io.*;
//import java.util.ArrayList;
//import java.util.Arrays;
//import java.util.List;
//import java.util.regex.Matcher;
//import java.util.regex.Pattern;
//import java.util.stream.Collectors;
//
//@Slf4j
//public class GenerateCommentCopy {
//    private static final String TARGET_FILE_PATH="D:\\Desktop\\DevProject\\saidake-manage-project\\sdk-service\\sdk-citi\\src\\main\\java\\com\\saidake\\citi\\controller\\TestStudentController.java";
//    private static final String FUNCTION_NAME="detailRequest";
//
//    public static void main(String[] args) throws IOException {
//        //a. 公共数据
//        Pattern commentStart = Pattern.compile("\\s*/\\*\\*\\s*");  //   /**
//        Pattern commentEnd = Pattern.compile("\\s*\\*/\\s*");       //   */
//        Pattern functionStartPattern = Pattern.compile(String.format("^[^\"]+%s\\s*\\([^\"]*?\\)\\s*\\{\\s*", FUNCTION_NAME));
//        Pattern commentNumberPattern = Pattern.compile("^\\s*//[1-9]\\.\\s.*?");  //支持1-4
//        List commentStorage=new ArrayList<>();
//        boolean functionStart = false;  // 函数是否开始
//        Integer leftCount = 0;          //  { 的个数
//        Integer rightCount = 0;         //  } 的个数
//        //a. 读取文件，添加注释
//        int lineIndex = 0;                             //  当前读取的行数
//        int commentStartLineNumber = 0;                //  注释开始的行数
//        int resultFunctionCommentLineNumber = 0;
//        //a. 读取目标文件，获取基础信息
//        BufferedReader bufferedReader = new BufferedReader(new FileReader(new File(TARGET_FILE_PATH)));
//        for ( String currentLine = bufferedReader.readLine();currentLine!=null;currentLine = bufferedReader.readLine(),lineIndex++){
//            //b. 移除当前行的字符串，统计 { 的个数
//            String checkCountLine = currentLine.replaceAll("\".*?\"", "");
//            //b. 公共数据
//            Matcher commentStartMatcher = commentStart.matcher(currentLine);
//            Matcher commentEndMatcher = commentEnd.matcher(currentLine);
//            Matcher functionStartMatcher = functionStartPattern.matcher(currentLine);
//            boolean isFirstFunctionStart=false;             // 函数首行开始
//            //b. 检测注释 开始，结束 位置
//            if (commentStartMatcher.find()) {
//                commentStartLineNumber=lineIndex;      //存储注释开始行数
//            }
//            boolean hasEndComment = commentEndMatcher.find();
//            //b. 当函数开始时
//            if(functionStartMatcher.find()){
//                functionStart=true;
//                isFirstFunctionStart=true;
//                if(hasEndComment){
//                    resultFunctionCommentLineNumber=commentStartLineNumber;
//                }else{
//                    resultFunctionCommentLineNumber=lineIndex;
//                }
//            }
//            //b. 当函数已经开始时， 并且不是首行
//            if(functionStart&&!isFirstFunctionStart){
//                // 计算是否到达函数底部
//                int currentLeftCount = checkCountLine.split("\\{").length;
//                currentLeftCount=currentLeftCount==0?0:currentLeftCount-1;
//                int currentRightCount = checkCountLine.split("}").length;
//                currentRightCount=currentRightCount==0?0:currentRightCount-1;
//                leftCount+=leftCount+currentLeftCount;
//                rightCount+=rightCount+currentRightCount;
//                // 检测序列号 1-4
//                Matcher commentNumberMatcher = commentNumberPattern.matcher(checkCountLine);
//                if(commentNumberMatcher.find()){
//                    Integer currentCommentNumber=Integer.valueOf(commentNumberMatcher.group(0).replaceAll("[^1-9]",""));
//                    List currentArray=commentStorage;
//                    int howDeep=0;
//                    while (currentArray.size()>0&&currentArray.get(currentArray.size()-1) instanceof ArrayList) { // ["ccc", ["a",["b","k"]], ["z","c"] ]         1 1         ["b","k"]    //3
//                        currentArray=(ArrayList)currentArray.get(currentArray.size()-1);
//                        howDeep++;
//                    }
//                    String currentCommentContent = checkCountLine.replaceAll("^\\s*//[1-9]\\.\\s.*?", "").trim();
//                    if(currentCommentNumber>howDeep){
//                        List newCommentList=new ArrayList();
//                        newCommentList.add(currentCommentContent);
//                        currentArray.add(newCommentList);
//                    }else{
//                        currentArray.add(currentCommentContent);
//                    }
//                }
//            }
//            //b. 当函数已经结束时
//            if(functionStart&&rightCount-leftCount==1){
//                functionStart=false;
//            }
//            //b. 当函数首次开始，并且没有注释时
//            if (isFirstFunctionStart && !hasEndComment) {
//                //c. 定义公共数据
//                StringBuilder stringBuilder = new StringBuilder();
//                String emptyString = currentLine.replaceAll("[^\\s].*?$", "");
//                String emptyStarString = currentLine.replaceAll("[^\\s].*?$", "")+" * ";
//                String paramString = currentLine.replaceAll(".*?\\(", "").replaceAll("\\).*?$", "");
//                String[] paramList = paramString.split(",");
//                List<String> paramNameList = Arrays.stream(paramList).map(item -> item.trim().replaceAll("^.*?\\s", "").trim()).collect(Collectors.toList());
//                String returnString = currentLine.replaceAll("\\(.*?", "").trim();
//                //c 添加注释
//                appendToStringBuilder(stringBuilder, emptyString + "/**");
//                paramNameList.forEach(item -> appendToStringBuilder(stringBuilder, emptyStarString + "@param\t" + item));
//                if (!"void".equals(returnString)) {
//                    appendToStringBuilder(stringBuilder, emptyStarString + "@return\t");
//                }
//                appendToStringBuilder(stringBuilder, emptyString + " */");
//                appendToStringBuilder(stringBuilder, currentLine);
//                return stringBuilder.toString() + System.lineSeparator();
//                //c. END
//            }
//            return currentLine + System.lineSeparator();
//            //b. END
//        }
//
//        //a. 写入目标文件
//        BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(new File(TARGET_FILE_PATH)));
//        if(resultFunctionCommentLineNumber!=0){
//            Pattern commentStartFilter = Pattern.compile("\\s*/\\*\\*");
//            FileUtil.readAndWriteFile(TARGET_FILE_PATH, TARGET_FILE_PATH, resultFunctionCommentLineNumber[0], currentLine -> {
//                System.out.println("currentLine: "+currentLine);
//                StringBuilder stringBuilder = new StringBuilder(currentLine);
//                Matcher matcher = commentStartFilter.matcher(currentLine);
//                matcher.find();
//                String commentStartEmpty=matcher.group(0);
//                commentStartEmpty=commentStartEmpty.replace("/**"," * ");
//                // ["ccc", ["a",["b","k"]], ["z","c"] ]         1 1         ["b","k"]    //3
//                appendToStringBuilder(stringBuilder, commentStartEmpty + "Function summary (generated by saidake-manage-project):");
//                handleAppendComment(stringBuilder,commentStorage,commentStartEmpty);
//                return stringBuilder.toString()+System.lineSeparator();
//            });
//        }
//        //a. END
//    }
//
//    private static void handleAppendComment(StringBuilder stringBuilder,List commentStorage,String commentStartEmpty){
//        commentStorage.forEach(item->{
//            if(item instanceof ArrayList){
//                handleAppendComment(stringBuilder,(ArrayList)item,commentStartEmpty+"    ");
//            }else{
//                appendToStringBuilder(stringBuilder, commentStartEmpty+item);
//            }
//        });
//
//    }
//
//    private static void appendToStringBuilder(StringBuilder stringBuilder, String str) {
//        stringBuilder.append(System.lineSeparator());
//        stringBuilder.append(str);
//    }
//}
