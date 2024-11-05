package com.simi.linkedListSubstraction;

import java.util.Arrays;
import java.util.LinkedList;

public class LinkedListSubtraction {

    public static void main(String[] args) {
        LinkedList<Integer> linkedList1=new LinkedList<>(Arrays.asList(9,8,7));
        LinkedList<Integer> linkedList2=new LinkedList<>(Arrays.asList(5,1,2));
        System.out.println(substractLinkedList(linkedList1,linkedList2));
        System.out.println("-------------------------------------------------");
        LinkedList<Integer> linkedList3=new LinkedList<>(Arrays.asList(5,1,2));
        LinkedList<Integer> linkedList4=new LinkedList<>(Arrays.asList(9,8,7));
        System.out.println(substractLinkedList(linkedList3,linkedList4));
        System.out.println("-------------------------------------------------");
        LinkedList<Integer> linkedList5=new LinkedList<>(Arrays.asList(5));
        LinkedList<Integer> linkedList6=new LinkedList<>(Arrays.asList(9,8,7));
        System.out.println(substractLinkedList(linkedList5,linkedList6));
        System.out.println("-------------------------------------------------");
        LinkedList<Integer> linkedList7=new LinkedList<>(Arrays.asList(9,8,7));
        LinkedList<Integer> linkedList8=new LinkedList<>(Arrays.asList(5));
        System.out.println(substractLinkedList(linkedList7,linkedList8));
    }

    private static Integer substractLinkedList(LinkedList<Integer> linkedList1, LinkedList<Integer> linkedList2){
        if(linkedList1.isEmpty()||linkedList2.isEmpty())throw new RuntimeException("invalid linked list");
        //  9  8  6
        Integer[] intList=new Integer[Math.max(linkedList1.size(), linkedList2.size())+1];
        //A. 公共数据。
        boolean backNum=false;
        Integer val1 = linkedList1.pollLast(), val2=linkedList2.pollLast();
        for(int i=intList.length-1;val1!=null||val2!=null;i--){
            if(val1==null)val1=0;
            if(val2==null)val2=0;
            //B. 结算之前的借位
            val1=backNum?val1-1:val1;
            //B. 当前是否需要借位
            if(val1<val2){
                val1+=10;
                backNum=true;
            }else{
                backNum=false;
            }
            //B. 当前位计算
            int comp = val1 - val2;
            intList[i]=comp;
            val1=linkedList1.pollLast();
            val2=linkedList2.pollLast();
        }
        //A. 拼接所有位
        StringBuilder sb=new StringBuilder();
        int sumResult=0;
        for (int i = 0; i < intList.length; i++) {
            Integer integer = intList[i];
            if(integer==null)continue;
            //B. 处理多借位
            if(backNum||sumResult!=0){
                //C. 获取不为null的位数，并归还借位
                int nonNullCount=intList.length-i-1;
                sumResult+=(backNum?integer-10:integer)*Math.pow(10,nonNullCount);
                backNum=false;
                //B. 没有多借位，直接拼接
            } else{
                sb.append(integer);
            }
        }
        return sb.isEmpty()?sumResult:Integer.parseInt(sb.toString());
    }
}
