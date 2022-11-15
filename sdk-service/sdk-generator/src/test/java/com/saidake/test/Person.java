package com.saidake.test;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class Person {
    private String name;
    private String code;

    private int age;

    public static String getTT(){
        return "tttdd";
    }
}
