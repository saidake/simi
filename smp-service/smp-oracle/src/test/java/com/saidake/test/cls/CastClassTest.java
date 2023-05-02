package com.saidake.test.cls;

import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.web.client.HttpServerErrorException;

public class CastClassTest {
    public static void main(String[] args)  {
        HttpServerErrorException httpServerErrorException = new HttpServerErrorException(HttpStatus.ACCEPTED);
        test(httpServerErrorException.getStatusCode());
        int[] arr=new int[]{1,2,3};
        test(1,arr);
    }

    private static void test(HttpStatusCode httpStatus){
        System.out.println(httpStatus.toString());
    }
    private static void test(int a, int... b){
        for (int i : b) {
            System.out.println(i);
        }
    }
}
