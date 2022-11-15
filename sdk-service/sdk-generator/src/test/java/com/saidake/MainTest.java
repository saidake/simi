package com.saidake;


import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Enums;
import com.google.common.base.Optional;
import com.google.common.hash.BloomFilter;
import com.google.common.hash.Funnels;
import com.google.gson.Gson;
import com.google.gson.annotations.SerializedName;
import com.saidake.common.core.util.data.BigDecimalUtils;
import com.saidake.common.core.util.data.RandomUtil;
import com.saidake.common.log.util.SysLogUtils;
import com.saidake.reflect.UserInvocationHandler;
import com.saidake.reflect.UserService;
import com.saidake.reflect.UserServiceImpl;
import com.saidake.test.Person;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.io.FileUtils;
import org.springframework.expression.EvaluationContext;
import org.springframework.expression.spel.support.StandardEvaluationContext;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

@Slf4j
public class MainTest {
    public static ThreadLocal<Boolean> alreadyMarked=new ThreadLocal<>();

    public static void main(String[] args) throws UnknownHostException {
        System.out.println("String[] test ".replaceAll("\\s+.*$",""));
//        List<String> list=Arrays.asList("222","333","444");
//        list.add("dddd");
//        System.out.println(list);
//        MultiValueMap<String,String> map=new LinkedMultiValueMap<>();
//        map.add("test","abc");
//        map.add("test","abcdd");
//        map.add("test2","abcddddd");
//        System.out.println(map);
//        System.out.println(InetAddress.getLocalHost().getHostName());
//        List<Person> personList=new ArrayList<>();
//        for (int i = 0; i < 1000; i++) {
//            if(i==103)personList.add(new Person("张飞ddd","xxxzouhao",23));
//            personList.add(new Person(RandomUtil.getRandomName(),RandomUtil.getRandomString(),RandomUtil.getRandomAge()));
//        }
//        //=========================================================================== 时间 测试 一
//        long startTime=System.currentTimeMillis();
//        Set<String> codeSet=new HashSet<>();
//        personList.forEach(item-> codeSet.add(item.getCode()));
//        System.out.println(codeSet.contains("xxxzouhao"));
//        System.out.println(codeSet.contains("xxxa"));
//        log.info("spend time: {}",System.currentTimeMillis()-startTime);
//        //=========================================================================== 时间 测试 二
//        long startTime2=System.currentTimeMillis();
//        BloomFilter<Integer> filter = BloomFilter.create(
//                Funnels.integerFunnel(),
//                1000,
//                0.01);
//        personList.forEach(item-> filter.put(item.getCode().hashCode()));
//        System.out.println(filter.mightContain("xxxzouhao".hashCode()));
//        System.out.println(filter.mightContain("xxxa".hashCode()));
//        log.info("spend time2: {}",System.currentTimeMillis()-startTime2);
        //=========================================================================== exception test
//      try{
//          Person person = new Person();
//          Person person2 = new Person();
//          throw  new RuntimeException("dddd");
//          boolean aaa = person2.getName().equals("aaa");
//          List<Person> personList=Arrays.asList(person,person2);
//          List<Person> collect = personList.stream().sorted(Comparator.comparing(Person::getName)).collect(Collectors.toList());
//          System.out.println(collect);
//      }catch (Exception e){
//          System.out.println(e.getMessage());
//          System.out.println(e.getCause());
//          System.out.println(e.getClass());
//          System.out.println(e.getLocalizedMessage());
//          System.out.println(e.getStackTrace());
//      }
//        //=========================================================================== serialize test
//        ObjectMapper objectMapper = new ObjectMapper();
//        Person person = objectMapper.readValue("{\"name\":\"东风\",\"age\":28,\"testTTT\":1666536839000}", Person.class);
//        System.out.println(objectMapper.writeValueAsString(person));
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
            String dddd="";
            switch (dddd){
                case "ddd":
                    System.out.println("ddd");
                    //C. dododod1
                    break;
                case   "dddb"   :
                    System.out.println("ddd");
                    //C. dododod2
                    //C. IF(AA)      fsfatrea  taann ta
                    if(matcher!=null) {
                        System.out.println("test");
                        //D. testtstst
                    }else{
                        System.out.println("lala");
                    }
                    break;
                default:
                    //C. dododod3
                    System.out.println("ddd");
                    break;
            }
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


    public static void test(
            String[] args)
    {
        //A. 测试内容sfffffffffffffffffff
        System.out.println(alreadyMarked.get());
        //B. 第二步骤sffffffffffffff
        alreadyMarked.set(false);
        //C. 第二步骤2fssssssssssss
    }



    public static void dddd(
            String[] args,
            String test)
            throws IOException
    {
        //A. 测试内容ddddddddddddddddddddd
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
