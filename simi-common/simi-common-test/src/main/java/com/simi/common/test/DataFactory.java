package com.simi.common.test;

import com.simi.common.util.data.RandomUtil;
import lombok.experimental.UtilityClass;

import java.util.ArrayList;
import java.util.List;

@UtilityClass
public class DataFactory {
    private int DEFAULT_LENGTH=10;
    private int DEFAULT_LOWER=0;
    private int DEFAULT_UPPER=9999;

    public static List<Integer> getIntList(){
        return getIntList(DEFAULT_LENGTH, DEFAULT_LOWER,DEFAULT_UPPER);
    }
    public static List<Integer> getIntList(int length, int lower, int upper){
        ArrayList<Integer> intList = new ArrayList<>();
        for (int i = 0; i < length; i++) {
            intList.add(RandomUtil.getRandomInt(lower,upper));
        }
        return intList;
    }
    public static int[] getPrimitiveIntArray(int length, int lower, int upper){
        int[] intArr=new int[length];
        for (int i = 0; i < length; i++) {
            intArr[i]=RandomUtil.getRandomInt(lower,upper);
        }
        return intArr;
    }
    public static int[] getPrimitiveIntArray(){
        return getPrimitiveIntArray(DEFAULT_LENGTH, DEFAULT_LOWER,DEFAULT_UPPER);
    }
}
