package com.saidake.test.data;

import com.saidake.support.data.ColorEnum;

import java.io.IOException;

public class StringTest {
    private static final String test="abcdefg";
    public static void main(String[] args) throws IOException {
        Integer a=9;
        Long b=8L;
        System.out.println(test.substring(0,-1));
        System.out.println("\\%");
        System.out.println(test.charAt(-1));
        System.out.println('a'+'b');
        char[] list =new char[]{'d','b','a'};
        System.out.println(list);
        System.out.println(String.join("",new String(list)));
    }
}
