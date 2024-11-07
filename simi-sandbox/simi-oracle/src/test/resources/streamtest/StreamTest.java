package com.simi.streamtest;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class StreamTest {
    private static List<Integer> list=new ArrayList<>(Arrays.asList(1,3,8,5,4,6,2,7));
    public static void main(String[] args)  {
        // stand
        Stream<Integer> stream = list.stream();
        Stream<Integer> integerStream = stream.map(item -> item + 1);
        List<Integer> collect = integerStream.collect(Collectors.toList());
        System.out.println(collect);
        // smp
        Spliterator<Integer> spliterator = Spliterators.spliterator(list, 0);
        com.simi.streamtest.Stream<Integer> smpStream = StreamSupport.stream(spliterator, false);
        smpStream.forEach(a->{
            System.out.println("consumer: "+a);
        });

    }

}
