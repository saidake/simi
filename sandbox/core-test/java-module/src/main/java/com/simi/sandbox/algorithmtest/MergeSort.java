package com.simi.sandbox.algorithmtest;

public class MergeSort {
    public static void mergeSort(int[] arr, int left, int right) {
        // Check whether the range is valid.
        if (left < right) {
            // Calculate the middle index (left <= middle < right)
            //   2 3 			left=2  right=3  middle=2
            //   2 3 4 			left=2  right=4  middle=3
            //   2 3 4 5		left=2  right=5  middle=3
            int middle = (left + right) / 2;
            // Partition the array to ranges [left, middle] and [middle+1, right].
            mergeSort(arr, left, middle);
            mergeSort(arr, middle+1, right);

            int lLen=middle-left+1;
            int rLen=right-middle;
            int[] lArr=new int[lLen]; // [left, middle]
            int[] rArr=new int[rLen]; // (middle, right]
            // Split elements from arr to 'lArr' and 'rArr'.
            for(int i=0; i<lLen; i++){
                lArr[i]=arr[left+i];
            }
            for(int i=0; i<rLen; i++){
                rArr[i]=arr[middle+1+i];
            }
            // Simply merge the sorted left and right partitions.
            int i=0,j=0,k=left;
            for(;i<lLen&&j<rLen; k++){
                if(lArr[i]<rArr[j]){
                    arr[k]=lArr[i];
                    i++;
                }else{
                    arr[k]=rArr[j];
                    j++;
                }
            }
            // Handle the remaining elements
            for(;i<lLen; i++,k++){
                arr[k]=lArr[i];
            }
            for(;j<rLen; j++,k++){
                arr[k]=rArr[j];
            }
        }
    }


    public static void main(String[] args) {
        int[] arr = {38, 27, 43, 3, 9, 82, 10};
        mergeSort(arr, 0, arr.length - 1);
        for (int num : arr) {
            System.out.print(num + " ");
        }
    }
}