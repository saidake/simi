package com.simi;

public class Queue {
    private static int FIXED_LENGTH=100;
    private int[] ints=new int[FIXED_LENGTH];
    private int start=0;
    boolean isFull=false;
    private int end=0;
    //[ 0 0 0 4 0 ]
    //      s
    //  e
    //
    public void put(int ele){
        if(end>=FIXED_LENGTH&&start>0){
            //B. insert new ele into the head of queue.
            end=0;
        }else if(isFull){
            throw new RuntimeException("no available space!");
        }
        ints[end]=ele; // 0 index
        end++; // end = 1


    }
    public int get(){
        if(start!=end&&start>=FIXED_LENGTH){
            start=0;
        }
        int result=ints[start];
        start++;
        isFull=start==end;

        if(isFull){
            throw new RuntimeException("no available space!");
        }
        return result;
    }
    public static void main(String[] args) {
    }
}
