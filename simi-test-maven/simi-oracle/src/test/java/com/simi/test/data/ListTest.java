package com.simi.test.data;

import com.google.common.collect.Lists;
import com.simi.support.data.DataFactory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

public class ListTest {
    public static void main(String[] args) {
        List<Integer> intlist = DataFactory.intlist();
        Map<String, Integer> collect = intlist.stream().collect(Collectors.toMap(Object::toString, Function.identity()));
        System.out.println(collect);
        collect.put("dd",2222);
        Collection<Integer> values = collect.values();
        ArrayList<Integer> integers = new ArrayList<>(values);
        integers.add(999999);
        System.out.println(integers);
    }

    private static void test(int a,int... list){
        for (int i : list) {
            System.out.println(i);
        }
    }
}
