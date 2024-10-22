package com.simi.algorithm.temp;

import lombok.extern.slf4j.Slf4j;

import java.util.Arrays;
import java.util.OptionalInt;

/**
 * https://leetcode.cn/problems/maximum-rows-covered-by-columns/description/
 * <pre>
 * Constraints:
 *     m == matrix.length
 *     n == matrix[i].length
 *     1 <= m, n <= 12
 *     matrix[i][j] is either 0 or 1.
 *     1 <= numSelect <= n
 * </pre>
 * 0 0 0    1 0 0 1 0
 * 1 0 1    0 0 0 1 0
 * 0 1 1    1 0 0 1 0
 * 0 0 1    0 0 1 0 1
 *          0 0 1 0 0
 *          0 1 0 0 1
 *          0 1 0 0 1
 */
public class MaximumRowsCoveredByColumns {
    /**
     * @param matrix    A binary matrix
     * @param numSelect The number of distinct columns from matrix
     * @return The maximum number of rows that can be covered
     */
    public int maximumRows(int[][] matrix, int numSelect) {
        //A. Get the number of '1' of each column.
        int rLen = matrix.length; // 1 <= m, n <= 12;
        int cLen = matrix[0].length;
        int[] decimalArr=new int[rLen];
        for (int i = 0; i < rLen; i++) {
            int dec=0;
            for (int j = 0; j < cLen; j++) {
                //B. the decimal number start from the left to right of the array (e.g. [0,1,1] -> 110 -> 6).
                if(matrix[i][j]==1)dec+=1<<j;
            }
            decimalArr[i]=dec;
        }
        //A. Remove a specified number of columns and check whether each row is empty according to the mask of each row.
        int maxNum=0;
        for (int i = 0; i < cLen; i++) {
            for (int j = i+1; j < cLen; j++) {
                int mask=1<<i+1<<j;
                int curNum=0;
                for (int dec : decimalArr) {
                    if((dec&mask)==0)curNum++;
                }
                maxNum=Math.max(maxNum,curNum);
            }
        }
        return maxNum;
    }

}