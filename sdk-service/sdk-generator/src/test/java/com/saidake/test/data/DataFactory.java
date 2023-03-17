package com.saidake.test.data;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class DataFactory {
    public static ThreadLocal<Boolean> alreadyMarked=new ThreadLocal<>();
    public static Person person(){
        return new Person(){{setName("test");setAge(22);}};
    }
    public static Person person(String name, Integer age){
        return new Person(){{setName(name);setAge(age);}};
    }

    public static List<Person> personList(){
        return new ArrayList<>(Arrays.asList(
                new Person(){{setName("aa");setAge(11);}},
                new Person(){{setName("bb");setAge(22);}},
                new Person(){{setName("cc");setAge(33);}},
                new Person(){{setName("zz");setAge(666464);}}
        ));
    }

    public static List<Integer> intlist(){
        return new ArrayList<>(Arrays.asList(1,2,3,4,5));
    }

}
