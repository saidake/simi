package com.simi;

import com.simi.common.test.DataFactory;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

public class MainTest {
    public static void main(String[] args) {
        List<Integer> intList = DataFactory.getIntList();
        int[] primitiveIntArray = DataFactory.getPrimitiveIntArray();
        List<Integer> list= Arrays.stream(primitiveIntArray).boxed().toList();
    }
}
