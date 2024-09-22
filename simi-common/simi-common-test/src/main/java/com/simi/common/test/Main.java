package com.simi.common.test;

import java.util.Arrays;
import java.util.Scanner;

// 注意类名必须为 Main, 不要有任何 package xxx 信息
public class Main {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        // 注意 hasNext 和 hasNextLine 的区别
        // 注意 while 处理多个 case
        int len = in.nextInt();
        int[] arr = new int[len];
        for(int i=0; i<len && in.hasNextInt(); i++){
            arr[i]=in.nextInt();
        }
        //A. sort the arr.
        Arrays.sort(arr);
        //A. common data.
        int max=arr[0];
        for(int i=1; i<len; i++){
            int cur=arr[i];
            if(cur==max)max*=2;
            else if(cur>max)max=cur;
        }
        System.out.println(max);
    }
}