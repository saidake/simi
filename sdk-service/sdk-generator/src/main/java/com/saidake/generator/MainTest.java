package com.saidake.generator;


import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class MainTest {
    public static ThreadLocal<Boolean> alreadyMarked=new ThreadLocal<>();

    /**
     * 测试函数
     *
     * @param	args
     * @return
     */
    public static void main(String[] args)  {
        //A. 测试内容
        System.out.println(alreadyMarked.get());
        //B. 第二步骤
        alreadyMarked.set(false);
        //C. 第二步骤2
        System.out.println(Boolean.FALSE.equals(alreadyMarked.get()));
        //C. ccc步骤
        //A. aaaa2步骤
        //A. aaaa3步骤
        //B. 第二步骤22223333
        //C. 第二步骤3
        //B. 第二步骤22223333
        //C. 第二步骤2
        //A. aaaa4步骤
        System.out.println(Integer.valueOf("A".charAt(0)));
        Pattern normalFunctionPattern = Pattern.compile("(public|private|protected)?\\s*(static)?\\s*[A-z]+\\s*main\\s*\\(");
        Matcher matcher = normalFunctionPattern.matcher("    public static void main(String[] args)  {");
        System.out.println(matcher.find());


        Pattern commentEnd = Pattern.compile("^\\s*?\\*/\\s*?$");  /**  */
        Matcher matcher2 = commentEnd.matcher("  */  ");
        System.out.println(matcher2.find());
    }


    public static void test(
            String[] args,
            String test)
            throws IOException
    {
        //A. 测试内容
        System.out.println(alreadyMarked.get());
        //B. 第二步骤
        alreadyMarked.set(false);
        //C. 第二步骤2
        System.out.println(Boolean.FALSE.equals(alreadyMarked.get()));
        //C. ccc步骤
        //A. aaaa2步骤
        //A. aaaa3步骤
        //B. 第二步骤testttt
        //C. 第二步骤3
        //B. 第二步骤testttt
        //C. 第二步骤2
        //A. aaaa4步骤
        System.out.println(Integer.valueOf("A".charAt(0)));
        Pattern normalFunctionPattern = Pattern.compile("(public|private|protected)?\\s*(static)?\\s*[A-z]+\\s*[A-z]+\\s*\\(");

        Matcher matcher = normalFunctionPattern.matcher("    public static void main(String[] args)  {");
        System.out.println(matcher.find());


        Pattern commentEnd = Pattern.compile("^\\s*\\*/\\s*$");  /**  */
        Matcher matcher2 = commentEnd.matcher("Pattern normalFunctionPattern = Pattern.compile(\"(public|private|protected)?\\\\s*(static)?\\\\s*[A-z]+\\\\s*main\\\\s*\\\\(\");");
        System.out.println(matcher2.find());
    }
}