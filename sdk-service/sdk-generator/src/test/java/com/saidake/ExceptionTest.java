package com.saidake;

import java.util.Arrays;
import java.util.List;

public class ExceptionTest {
    public static void main(String[] args) {
        List<Integer> test= Arrays.asList(1,2,3);
        if(test.size()>5){
            System.out.println("ddd");
            throw new RuntimeException("666");
        }
        if(test.size()>1){
            throw new RuntimeException("ddd");
        }
    }
}
