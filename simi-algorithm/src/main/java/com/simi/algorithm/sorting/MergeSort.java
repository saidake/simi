package com.simi.algorithm.sorting;
import java.util.Arrays;
public class MergeSort {
    public static void main(String[] args) {
        int[] ints = {1, 9, 3, 4};
        mergeSort(ints, 0, 4);
        Arrays.stream(ints).forEach(System.out::println);
    }
    private static void mergeSort(int[] arr, int left, int right){
        int middle = (left+right)/2;
        mergeSort(arr, left, middle);
        mergeSort(arr, middle+1, right);
        merge(arr, left, middle, right);
    }
    public static void merge(int[] arr, int left, int middle, int right) {
        //A. create two arrays for both left and right partitions and store elements in them.
        int lLen=middle-left+1;
        int rLen=right-middle+1;
        int[] lArr=new int[lLen];
        int[] rArr=new int[rLen];
        for (int i = 0; i < lLen; i++)
            lArr[i]=arr[left+i];
        for (int j = 0; j<rLen; j++)
            rArr[j]=arr[right+j];
        //A. place the values with smaller values on the left side of the arr.
        int i=left, j=middle, k=0;
        while (i<lLen && j<rLen){
            if(lArr[i]<rArr[j]){
                arr[k]=lArr[i];
                i++;
            }else{
                arr[k]=rArr[j];
                j++;
            }
            k++;
        }
        //A. Append the remaining items to the tail.
        while (i<lLen){
            arr[k]=lArr[i];
            k++;
            i++;
        }
        while (j<rLen){
            arr[j]=rArr[j];
            j++;
            k++;
        }
    }

}
