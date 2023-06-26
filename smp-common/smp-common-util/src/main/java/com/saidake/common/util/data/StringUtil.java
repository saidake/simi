package com.saidake.common.util.data;

import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class StringUtil {
    private static Pattern linePattern = Pattern.compile("_(\\w)");   //  \\w 非特殊符号
    private static Pattern humpPattern = Pattern.compile("[A-Z]");


    public static Integer countSubString(String source,String sub){
        Integer resultCount=0;
        for (int i=0,next=0;(next=source.indexOf(sub,i))>=0;i=next+sub.length()){
            resultCount++;
        }
        return resultCount;
    }

    /**
     * 首字母大写
     * @param str
     * @return
     */
    public static String startUpper(String str) {
        char[] chars = str.toCharArray();
        char start=chars[0];
        if(chars.length==0){
            throw new RuntimeException("empty str");
        };
        if(start>=97&&start<=122){
            start=(char)(start-32);
        }
        chars[0]=start;
        return String.valueOf(chars);
    }

    /**
     * 首字母小写
     * @param str
     * @return
     */
    public static String startLower(String str) {
        char[] chars = str.toCharArray();
        char start=chars[0];
        if(chars.length==0)return null;
        if(start>=65&&start<=90){
            start=(char)(start+32);
        }
        chars[0]=start;
        return String.valueOf(chars);
    }

    /**
     * 大写转变量 CONTACT & CREDIT  ==> contactCredit
     * @param str
     * @return
     */
    public static String convertUpperString(String str,Boolean isStartUpper) {
        String[] split = str.split("[^A-z]+");
        List<String> stringList=new ArrayList<>();
        for (int i = 0; i < split.length; i++) {
            if(i==0&&!isStartUpper){
                stringList.add(split[i].toLowerCase());
                continue;
            }
            stringList.add(startUpper(split[i].toLowerCase()));
        }
        return StringUtils.join(stringList);
    }

    /**
     * 驼峰转下划线,最后转为大写
     * @param str
     * @return
     */
    public static String humpToLine(String str) {
        Matcher matcher = humpPattern.matcher(str);
        StringBuffer sb = new StringBuffer();
        while (matcher.find()) {
            matcher.appendReplacement(sb, "_" + matcher.group(0).toUpperCase());
        }
        matcher.appendTail(sb);
        return sb.toString().toUpperCase();
    }

    /**
     * 下划线转驼峰,正常输出
     * @param str
     * @return
     */
    public static String lineToHump(String str) {
        Matcher matcher = linePattern.matcher(str);
        StringBuffer sb = new StringBuffer();
        while (matcher.find()) {
            matcher.appendReplacement(sb, matcher.group(1).toUpperCase());
        }
        matcher.appendTail(sb);
        return sb.toString();
    }

}
