package com.simi.test.data;

import com.google.common.collect.Lists;
import com.simi.support.data.DataFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class ListTest {
    public static void main(String[] args) {
        test(2,new int[]{1,2,3,4});
        List<Integer> intlist = DataFactory.intlist();
        List<Integer> intlist2 = DataFactory.intlist();
        List<Integer> intlist3 = DataFactory.intlist();
        List<List<Integer>> lists=new ArrayList<>();
        lists.add(intlist);
        lists.add(intlist2);
        lists.add(intlist3);
        //flat map
        System.out.println(lists.stream().flatMap(List::stream).collect(Collectors.toList()));;
        intlist.add(null);
        intlist.add(null);
        System.out.println(intlist);
    }

    private static void test(int a,int... list){
        for (int i : list) {
            System.out.println(i);
        }
    }
}
