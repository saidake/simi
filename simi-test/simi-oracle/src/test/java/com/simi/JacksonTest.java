package com.simi;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.simi.support.data.ColorEnum;
import com.simi.support.data.Person;
import com.simi.support.data.Toy;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import java.time.Instant;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;
import java.util.stream.IntStream;


@Slf4j
public class JacksonTest {
    public static void main(String[] args) throws JsonProcessingException {
        long startTime=System.currentTimeMillis();
        List<Integer> collect = IntStream.range(1, 43).boxed().collect(Collectors.toList());
        ThreadPoolTaskExecutor threadPoolTaskExecutor = new ThreadPoolTaskExecutor();
        List<CompletableFuture> completableFutures=new ArrayList<>();
        for (Integer integer : collect) {
            CompletableFuture<Void> voidCompletableFuture = CompletableFuture.runAsync(() -> {
                System.out.println("current integer");
                try {
                    Thread.sleep(500);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
            });
            completableFutures.add(voidCompletableFuture);
        }
        CompletableFuture.allOf(completableFutures.toArray(new CompletableFuture[completableFutures.size()])).join();
        System.out.println(System.currentTimeMillis()-startTime);
    }
    private static void enumSerialize() throws JsonProcessingException {
        System.out.println(ColorEnum.CYAN.getStrVal());
        System.out.println(ColorEnum.CYAN.getDoubleVal());
        System.out.println(ColorEnum.CYAN.getIntVal());
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.configure(DeserializationFeature.ACCEPT_EMPTY_STRING_AS_NULL_OBJECT,true);
        //序列化
        String colorString = objectMapper.writeValueAsString(ColorEnum.RED);
        System.out.println(colorString);
        System.out.println("枚举序列化："+colorString);
        //反序列化
        ColorEnum colorEnum = objectMapper.readValue(colorString, ColorEnum.class);
        System.out.println("枚举反序列化："+colorEnum);
        ColorEnum colorEnum2333 = objectMapper.readValue(ColorEnum.RED.name(), ColorEnum.class);
        System.out.println("枚举反序列化2："+colorEnum2333);
        ColorEnum colorEnum2 = objectMapper.readValue("RED", ColorEnum.class);
        System.out.println("枚举反序列化3："+colorEnum2);
        ColorEnum colorEnum3 = objectMapper.readValue("dddd", ColorEnum.class);
        System.out.println("枚举反序列化4："+colorEnum3);
    }



    private static void objectSerialize() throws JsonProcessingException {
        System.out.println(LocalDateTime.now().toString());
        Person person = new Person("dd",2,new Toy("d","d"),ColorEnum.BLUE, Instant.now(),2);
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.configure(DeserializationFeature.ACCEPT_EMPTY_STRING_AS_NULL_OBJECT,true);
        //序列化
        String personStr = objectMapper.writeValueAsString(person);
        System.out.println(personStr);
        System.out.println("序列化："+personStr);
        //反序列化
        Person personResult = objectMapper.readValue(personStr, Person.class);
        System.out.println("反序列化："+personResult);
        Person personResult2 = objectMapper.readValue(personStr, Person.class);
        System.out.println("反序列化2："+personResult2);
        Person personResult3 = objectMapper.convertValue("{\"name\":\"dd\",\"age\":null,\"tdd\":null\"toy\":{\"name\":null,\"type\":null}}", Person.class);
        System.out.println("反序列化3："+personResult3);
    }
}
