package com.simi;

public class SingletonClass {
    private SingletonClass(){

    }
    private volatile static Person person;

    public static Person getSingletonClass(){
        if(person==null){
            synchronized (SingletonClass.class){
                if(person==null){
                    person=new Person();
                }
            }
        }
        return person;
    }

}
