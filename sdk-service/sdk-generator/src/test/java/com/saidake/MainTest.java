package com.saidake;


import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang.ArrayUtils;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

@Slf4j
public class MainTest {

    private static boolean test(int a,int... b){
        for (int i : b) {
            System.out.println(i);
        }
        return true;
    }
    public static void main(String[] args) {
        int testdd=9;
        System.out.println(testdd==5?test(testdd,1,2,3):23333);
        //=================================================================== 异常测试
        try{
            try{
                int a=999;
                try{
                    a+=1;
                    personList.stream().forEach(per->{
                        per.setAge(null);
                        per.setAge(per.getAge()+0);
                    });
                    a+=2;
                }catch (Exception e){
                    throw new RuntimeException(e);
                }
            }catch (Exception e){
                throw new SdkException(e);
            }
        }catch (Exception e){
            StringWriter stringWriter = new StringWriter();
            e.printStackTrace(new PrintWriter(stringWriter));
            System.out.println("stackTraceElement=================");
            System.out.println(stringWriter);
        }
    }

    public static class SdkException extends RuntimeException{
        SdkException(Exception e){
            super(e);
        }
    }

    public interface FatherFace<T, Z>{
        void fatherFunc(double size);
        default void defaultFunc(){}
    }

    public interface MotherFace{
        void motherFunc(double value);
        default void defaultFunc(){}
    }

    public interface SonFace<T, Z> extends FatherFace<T, Z> {
        int CC=77;
        void accept(T t);
        default void sonDefault(){
        }
        interface SonInsideFace extends SonFace<Double,Integer>, MotherFace {
            @Override
            default void accept(Double i) {
            }
            @Override
            default void defaultFunc() {
            }
        }
    }

    @Getter
    @Setter
    public static class Person{
        public String name;
        public Integer age;
    }
    public static ThreadLocal<Boolean> alreadyMarked=new ThreadLocal<>();
    public static List<Integer> intlist=new ArrayList<>(Arrays.asList(1,2,3,4,5));
    public static List<Integer> emptyIntList=new ArrayList<>();
    public static List<Person> personList=new ArrayList<>(Arrays.asList(
            new Person(){{setName("bb");setAge(22);}},
            new Person(){{setName("aa");setAge(11);}},
            new Person(){{setName("cc");setAge(33);}},
            new Person(){{setName("aa");setAge(666464);}}
            ));
    public static List<Person> emptyPersonList=new ArrayList<>();

}
