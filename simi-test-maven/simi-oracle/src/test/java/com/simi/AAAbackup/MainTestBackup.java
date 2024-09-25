//package com.simi;
//
//
//import lombok.extern.slf4j.Slf4j;
//
//import java.io.IOException;
//import java.net.UnknownHostException;
//import java.util.regex.Matcher;
//import java.util.regex.Pattern;
//
//@Slf4j
//public class MainTest {
//    public static ThreadLocal<Boolean> alreadyMarked=new ThreadLocal<>();
//
//    public static void main(String[] args) throws UnknownHostException {
//        System.out.println("String[] test ".replaceAll("\\s+.*$",""));
//    }
//
//    public static String test(
//            String[] args,
//            String test)
//            throws IOException
//    {
//        //A. 测试内容
//        System.out.println(alreadyMarked.get());
//        //B. 第二步骤
//        alreadyMarked.set(false);
//        //C. 第二步骤2
//        System.out.println(Boolean.FALSE.equals(alreadyMarked.get()));
//        //C. ccc步骤
//        //A. aaaa2步骤
//        //A. aaaa3步骤
//        //B. 第二步骤testttt
//        //C. 第二步骤3
//        //B. 第二步骤testttt
//        //C. 第二步骤2
//        //A. aaaa4步骤
//        System.out.println(Integer.valueOf("A".charAt(0)));
//        Pattern normalFunctionPattern = Pattern.compile("(public|private|protected)?\\s*(static)?\\s*[A-z]+\\s*[A-z]+\\s*\\(");
//
//        Matcher matcher = normalFunctionPattern.matcher("    public static void main(String[] args)  {");
//        System.out.println(matcher.find());
//
//        //A. IF(AA)      从formData里获取
//        if(matcher!=null){
//            System.out.println("test");
//            String dddd="";
//            switch (dddd){
//                case "ddd":
//                    System.out.println("ddd");
//                    //C. dododod1
//                    break;
//                case   "dddb"   :
//                    System.out.println("ddd");
//                    //C. dododod2
//                    //C. IF(AA)      fsfatrea  taann ta
//                    if(matcher!=null) {
//                        System.out.println("test");
//                        //D. testtstst
//                    }else{
//                        System.out.println("lala");
//                    }
//                    break;
//                default:
//                    //C. dododod3
//                    System.out.println("ddd");
//                    break;
//            }
//        }else if(matcher!=null){
//            System.out.println("dd");
//        }else{
//            System.out.println("else");
//            if(matcher!=null) {
//                System.out.println("test");
//                //D. testtstst
//            }else{
//                System.out.println("lala");
//            }
//        }
//        String dddd="";
//
//        //A. SWITCH:   获取form信息 用searchType
//        switch (dddd){
//            case "ddd":
//                System.out.println("ddd");
//                //B. dododod1
//                break;
//            case   "dddb"   :
//                System.out.println("ddd");
//                //B. dododod2
//                break;
//            default:
//                //B. dododod3
//                System.out.println("ddd");
//                break;
//        }
//        if(matcher!=null) {
//            System.out.println("test");
//            //D. testtstst
//        }else{
//            System.out.println("lala");
//        }
//        Pattern commentEnd = Pattern.compile("(?<=case)\\s*?[A-z1-9.\"]+\\s*?(?=:)");  /**  */
//        Matcher matcher2 = commentEnd.matcher("            case   \"dddb\"   :    ");
//        System.out.println(matcher2.find());
//        if(matcher!=null) {
//            System.out.println("test");
//            //D. testtstst
//        }else{
//            System.out.println("lala");
//        }
//        return null;
//    }
//
//
//    public static void test(
//            String[] args)
//    {
//        //A. 测试内容sfffffffffffffffffff
//        System.out.println(alreadyMarked.get());
//        //B. 第二步骤sffffffffffffff
//        alreadyMarked.set(false);
//        //C. 第二步骤2fssssssssssss
//    }
//
//
//
//    public static void dddd(
//            String[] args,
//            String test)
//            throws IOException
//    {
//        //A. 测试内容ddddddddddddddddddddd
//        System.out.println(alreadyMarked.get());
//        //B. 第二步骤
//        alreadyMarked.set(false);
//        //C. 第二步骤2
//        System.out.println(Boolean.FALSE.equals(alreadyMarked.get()));
//        //C. ccc步骤
//        //A. aaaa2步骤
//        //A. aaaa3步骤
//        /*
//        dddddddddddddddddd
//         */
//        //B. 第二步骤testttt
//        //C. 第二步骤3
//        //B. 第二步骤testttt
//        //C. 第二步骤2
//        //A. aaaa4步骤
//        System.out.println(Integer.valueOf("A".charAt(0)));
//        Pattern normalFunctionPattern = Pattern.compile("(public|private|protected)?\\s*(static)?\\s*[A-z]+\\s*[A-z]+\\s*\\(");
//
//        Matcher matcher = normalFunctionPattern.matcher("    public static void main(String[] args)  {");
//        System.out.println(matcher.find());
//
//
//        Pattern commentEnd = Pattern.compile("^\\s*\\*/\\s*$");  /**  */
//        Matcher matcher2 = commentEnd.matcher("Pattern normalFunctionPattern = Pattern.compile(\"(public|private|protected)?\\\\s*(static)?\\\\s*[A-z]+\\\\s*main\\\\s*\\\\(\");");
//        System.out.println(matcher2.find());
//    }
//}
