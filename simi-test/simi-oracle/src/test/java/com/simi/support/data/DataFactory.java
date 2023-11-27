package com.simi.support.data;

import java.util.*;

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
    public static Map<Integer,Integer> intMap(){
        HashMap<Integer,Integer> objectObjectHashMap = new HashMap<>();
        objectObjectHashMap.put(1,2);
        objectObjectHashMap.put(2,3);
        objectObjectHashMap.put(4,5);
        objectObjectHashMap.put(5,6);
        return objectObjectHashMap;
    }

}
