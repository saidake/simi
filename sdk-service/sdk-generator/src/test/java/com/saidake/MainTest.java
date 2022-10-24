package com.saidake;


import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.gson.Gson;
import com.google.gson.annotations.SerializedName;
import com.saidake.common.log.util.SysLogUtils;
import com.saidake.reflect.UserInvocationHandler;
import com.saidake.reflect.UserService;
import com.saidake.reflect.UserServiceImpl;
import lombok.Data;
import org.apache.commons.beanutils.BeanUtils;
import org.springframework.expression.EvaluationContext;
import org.springframework.expression.spel.support.StandardEvaluationContext;
import org.springframework.format.annotation.DateTimeFormat;

import java.io.IOException;
import java.util.Date;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class MainTest {
    public static ThreadLocal<Boolean> alreadyMarked=new ThreadLocal<>();


    @Data
    private static class Person{
        private String name;

        private int age;

        @JsonProperty("test_date")
        @JsonAlias({"testTTT"})
        @JsonFormat(shape = JsonFormat.Shape.STRING,pattern = "yyyy-MM-dd HH:mm:ss")
        private Date testDate;

        public static String getTT(){
            return "tttdd";
        }
    }



    public static void main(String[] args) throws JsonProcessingException{
        //=========================================================================== serialize test
        ObjectMapper objectMapper = new ObjectMapper();
        Person person = objectMapper.readValue("{\"name\":\"东风\",\"age\":28,\"testTTT\":1666536839000}", Person.class);
        System.out.println(objectMapper.writeValueAsString(person));
        //=========================================================================== type test
//        System.out.println((double)2/3);
//        System.out.println(new DecimalFormat("#.##").format(345.6666666));
//        System.out.println(new DecimalFormat("##,##.##").format(345.6666666));
//        System.out.println(new DecimalFormat("##,##.##").format(0.6666666));
//        System.out.println(new DecimalFormat("00,00.00").format(345.6666666));
//        System.out.println(new DecimalFormat("00,00.00").format(0.6666666));
        //=========================================================================== reflect test
//        UserService userService=new UserServiceImpl();
//        UserService proxy=(UserService) Proxy.newProxyInstance(
//                MainTest.class.getClassLoader(),
//                new Class[]{UserService.class},
//                new UserInvocationHandler(userService));
//        proxy.query();
        //=========================================================================== spel test
//        EvaluationContext context = new StandardEvaluationContext();
//        Person dddd = new Person() {{
//            setAge(23);
//            setName("dddd");
//        }};
//        context.setVariable("a", 123);
//        System.out.println(SysLogUtils.getValue(context,"{#a}",Integer.class));
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

        //A. IF(AA)      从formData里获取
        if(matcher!=null){
            System.out.println("test");
        }else if(matcher!=null){
            System.out.println("dd");
        }else{
            System.out.println("else");
        }
        String dddd="";

        //A. SWITCH:   获取form信息 用searchType
        switch (dddd){
            case "ddd":
                System.out.println("ddd");
                //B. dododod1
                break;
            case   "dddb"   :
                System.out.println("ddd");
                //B. dododod2
                break;
            default:
                //B. dododod3
                System.out.println("ddd");
                break;
        }

        Pattern commentEnd = Pattern.compile("(?<=case)\\s*?[A-z1-9.\"]+\\s*?(?=:)");  /**  */
        Matcher matcher2 = commentEnd.matcher("            case   \"dddb\"   :    ");
        System.out.println(matcher2.find());

    }


    public static void dddd(
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
        /*
        dddddddddddddddddd
         */
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
