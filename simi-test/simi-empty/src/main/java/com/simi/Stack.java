package com.simi;

public class Stack {
    private int[] ints=new int[100];
    private int cursor=0;
    //[ 1 2 3 4 ]
    //    ~
    public void put(int ele){
        if(cursor>=ints.length)throw new RuntimeException("stack full error");
        ints[cursor]=ele;
        cursor++;
    }
    public int get(){
        if(cursor<0)throw new RuntimeException("stack length error");
        cursor--;
        return ints[cursor];
    }
    public static void main(String[] args) {

    }
}
