package com.simi.getdata;


import java.util.Arrays;
import java.util.List;

public class GetDataInBatches {
    public static void main(String[] args) {
        BService bService=new BService();
        //A. get results.
        List<Integer> strings = bService.get(
                Arrays.asList(1, 2, 3, 4, 5,6,7,8,9,10,1, 2, 3, 4, 5,6,7,8,9,10)
        );
        System.out.println(strings);
    }
}
