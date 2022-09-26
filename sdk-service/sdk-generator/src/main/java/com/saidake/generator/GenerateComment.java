package com.saidake.generator;
import com.saidake.common.core.util.file.FileUtil;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class GenerateComment {
    private static final String TARGET_FILE_PATH="D:\\Desktop\\DevProject\\saidake-manage-project\\sdk-service\\sdk-generator\\src\\test\\java\\MainTest.java";
    private static final String FUNCTION_NAME="main";

    public static void main(String[] args) {
        //1. 公共数据
        Pattern commentStart = Pattern.compile("\\s*/\\*\\*\\s*");
        Pattern commentEnd = Pattern.compile("\\s*\\*/\\s*");  /**  */
        Pattern functionStartPattern = Pattern.compile(String.format("^[^\"]+%s\\s*\\([^\"]*?\\)\\s*\\{\\s*", FUNCTION_NAME));
        Pattern commentNumberPattern = Pattern.compile("^\\s*//[1-9]\\.\\s.*?");  //支持1-4
        List commentStorage=new ArrayList<>();
        final Boolean[] hasComment = new Boolean[]{false};
        final Boolean[] hasEndComment = new Boolean[]{false};
        final Boolean[] functionStart = new Boolean[]{false};  // 函数开始
        final Integer[] leftCount = new Integer[]{0};     //  { 的个数
        final Integer[] rightCount = new Integer[]{0};    //  } 的个数
        //1. 添加注释
        final int[] markLineNumber = {0};
        final int[] commentStartLineNumber = {0};
        final int[] resultFunctionCommentLineNumber = {0};
        FileUtil.readAndWriteFile(TARGET_FILE_PATH, TARGET_FILE_PATH, currentLine -> {
            markLineNumber[0]++;
            //2. 移除当前行的字符串，统计 { 的个数
            String checkCountLine = currentLine.replaceAll("\".*?\"", "");
            //2. 公共数据
            Matcher commentStartMatcher = commentStart.matcher(currentLine);
            Matcher commentEndMatcher = commentEnd.matcher(currentLine);
            Matcher functionStartMatcher = functionStartPattern.matcher(currentLine);
            boolean isFirstFunctionStart=false;
            //2. 当注释开始时
            if (commentStartMatcher.find()) {
                hasComment[0] = true;
                commentStartLineNumber[0]=markLineNumber[0];
            }
            //2. 当函数开始时
            if(functionStartMatcher.find()){
                functionStart[0]=true;
                isFirstFunctionStart=true;
                if(hasComment[0]){
                    resultFunctionCommentLineNumber[0]=commentStartLineNumber[0]-1;
                }else{
                    resultFunctionCommentLineNumber[0]=markLineNumber[0];
                }
            }

            if (commentEndMatcher.find() && hasComment[0]) {
                hasEndComment[0] = true;
            }
            //2. 当函数已经开始时， 并且不是首行
            if(functionStart[0]&&!isFirstFunctionStart){
                // 计算是否到达函数底部
                int currentLeftCount = checkCountLine.split("\\{").length;
                currentLeftCount=currentLeftCount==0?0:currentLeftCount-1;
                int currentRightCount = checkCountLine.split("}").length;
                currentRightCount=currentRightCount==0?0:currentRightCount-1;
                leftCount[0]+=leftCount[0]+currentLeftCount;
                rightCount[0]+=rightCount[0]+currentRightCount;
                // 检测序列号 1-4
                Matcher commentNumberMatcher = commentNumberPattern.matcher(checkCountLine);
                if(commentNumberMatcher.find()){
                    Integer currentCommentNumber=Integer.valueOf(commentNumberMatcher.group(0).replaceAll("[^1-9]",""));
                    List currentArray=commentStorage;
                    int howDeep=0;
                    while (currentArray.size()>0&&currentArray.get(currentArray.size()-1) instanceof ArrayList) { // ["ccc", ["a",["b","k"]], ["z","c"] ]         1 1         ["b","k"]    //3
                        currentArray=(ArrayList)currentArray.get(currentArray.size()-1);
                        howDeep++;
                    }
                    String currentCommentContent = checkCountLine.replaceAll("^\\s*//[1-9]\\.\\s.*?", "").trim();
                    if(currentCommentNumber>howDeep){
                        List newCommentList=new ArrayList();
                        newCommentList.add(currentCommentContent);
                        currentArray.add(newCommentList);
                    }else{
                        currentArray.add(currentCommentContent);
                    }
                }
            }
            //2. 当函数已经结束时
            if(functionStart[0]&&rightCount[0]-leftCount[0]==1){
                functionStart[0]=false;
            }
            //2. 当函数首次开始，并且没有注释时
            if (isFirstFunctionStart && !hasEndComment[0] && !hasComment[0]) {
                //3. 定义公共数据
                StringBuilder stringBuilder = new StringBuilder();
                String emptyString = currentLine.replaceAll("[^\\s].*?$", "");
                String emptyStarString = currentLine.replaceAll("[^\\s].*?$", "")+" * ";
                String paramString = currentLine.replaceAll(".*?\\(", "").replaceAll("\\).*?$", "");
                String[] paramList = paramString.split(",");
                List<String> paramNameList = Arrays.stream(paramList).map(item -> item.trim().replaceAll("^.*?\\s", "").trim()).collect(Collectors.toList());
                String returnString = currentLine.replaceAll("\\(.*?", "").trim();
                //3 添加注释
                appendToStringBuilder(stringBuilder, emptyString + "/**");
                paramNameList.forEach(item -> appendToStringBuilder(stringBuilder, emptyStarString + "@param\t" + item));
                if (!"void".equals(returnString)) {
                    appendToStringBuilder(stringBuilder, emptyStarString + "@return\t");
                }
                appendToStringBuilder(stringBuilder, emptyString + " */");
                appendToStringBuilder(stringBuilder, currentLine);
                return stringBuilder.toString() + System.lineSeparator();
                //3. END
            }
            return currentLine + System.lineSeparator();
            //2. END
        });

        if(resultFunctionCommentLineNumber[0]!=0){

        }
        Pattern commentStartFilter = Pattern.compile("\\s*/\\*\\*");
        FileUtil.readAndWriteFile(TARGET_FILE_PATH, TARGET_FILE_PATH, resultFunctionCommentLineNumber[0], currentLine -> {
            System.out.println("currentLine: "+currentLine);
            StringBuilder stringBuilder = new StringBuilder(currentLine);
            Matcher matcher = commentStartFilter.matcher(currentLine);
            matcher.find();
            String commentStartEmpty=matcher.group(0);
            commentStartEmpty=commentStartEmpty.replace("/**"," * ");
            // ["ccc", ["a",["b","k"]], ["z","c"] ]         1 1         ["b","k"]    //3
            appendToStringBuilder(stringBuilder, commentStartEmpty + "Function summary (generated by saidake-manage-project):");
            handleAppendComment(stringBuilder,commentStorage,commentStartEmpty);
            return stringBuilder.toString()+System.lineSeparator();
        });
        //1. END
    }

    private static void handleAppendComment(StringBuilder stringBuilder,List commentStorage,String commentStartEmpty){
        commentStorage.forEach(item->{
            if(item instanceof ArrayList){
                handleAppendComment(stringBuilder,(ArrayList)item,commentStartEmpty+"    ");
            }else{
                appendToStringBuilder(stringBuilder, commentStartEmpty+item);
            }
        });

    }

    private static void appendToStringBuilder(StringBuilder stringBuilder, String str) {
        stringBuilder.append(System.lineSeparator());
        stringBuilder.append(str);
    }
}
