package com.saidake;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.saidake.test.data.Person;
import com.saidake.test.data.Toy;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class JacksonTest {
    public static void main(String[] args) throws JsonProcessingException {
        Person person = new Person("dd",2,new Toy("d","d"),2);
        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.configure(DeserializationFeature.ACCEPT_EMPTY_STRING_AS_NULL_OBJECT,true);
        //序列化
        String personStr = objectMapper.writeValueAsString(person);
        System.out.println(personStr);
        System.out.println("序列化："+personStr);
        //反序列化
        Person personResult = objectMapper.readValue(personStr, Person.class);
        System.out.println("反序列化："+personResult);
        Person personResult2 = objectMapper.readValue("{\"name\":\"dd\",\"age\":null,\"tdd\":null\"toy\":{\"name\":null,\"type\":null}}", Person.class);
        System.out.println("反序列化2："+personResult2);
    }
}
