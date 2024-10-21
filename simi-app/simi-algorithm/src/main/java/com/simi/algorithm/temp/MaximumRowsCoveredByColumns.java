package com.simi.algorithm.temp;

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
        int[] cNumList = new int[cLen];
        int[] selectedC = new int[numSelect];
        for (int i = 0; i < rLen; i++) {
            for (int j = 0; j < cLen; j++) {
                int item = matrix[i][j];
                if (item == 1) cNumList[j] += 1;
            }
        }

        return 0;
    }

    public void dfs(int[][] matrix, int numSelect) {
    }
}