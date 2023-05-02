package com.saidake.test.data;

import com.saidake.support.data.ColorEnum;
import com.saidake.support.data.DataFactory;

import java.io.IOException;
import java.util.Map;

public class MapTest {

    public static void main(String[] args) throws IOException {
        Map<Integer, Integer> integerIntegerMap = DataFactory.intMap();
        for (Map.Entry<Integer, Integer> integerIntegerEntry : integerIntegerMap.entrySet()) {
            if(integerIntegerEntry.getKey().equals(2)){
                integerIntegerEntry.setValue(99999);
            }
        }
        System.out.println(integerIntegerMap);
    }
}
