package com.simi.algorithm.binary;

import java.util.ArrayList;
import java.util.List;

/**
 * https://leetcode.cn/problems/maximum-rows-covered-by-columns/description/
 * Maximum Rows Covered by Columns
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
        //A. Convert each row of the matrix to a bitmask representing columns with '1'
        for (int i = 0; i < rLen; i++) {
            int dec=0;
            for (int j = 0; j < cLen; j++) {
                //B. The decimal number start from the left to right of the array (e.g. [0,1,1] -> 110 -> 6).
                if(matrix[i][j]==1)dec+=1<<j;
            }
            decimalArr[i]=dec;
        }
        //A. Generate all combinations of columns of size numSelect
        List<Integer> combinations = new ArrayList<>();
        generateCombinations(0, 0, numSelect, cLen, combinations);

        //A. Try each combination to find the maximum number of covered rows
        int maxNum = 0;
        for (int combination : combinations) {
            int curNum = 0;
            for (int rowMask : decimalArr) {
                if ((rowMask & combination) == rowMask) {
                    //B. Row is covered if all its '1' columns are included in the selected combination
                    curNum++;
                }
            }
            maxNum = Math.max(maxNum, curNum);
        }

        return maxNum;
    }
    /**
     * Generate all combinations of selecting numSelect columns from cLen columns.
     * [[0,0,0],[1,0,1],[0,1,1]]  numSelect = 2   cLen=3
     *
     * generateCombinations(0, 0, 2, 3, combinations);
     * start-current
     *                         0-0
     *            0-1          1-2      2-4
     *         1-3   2-5       2-6      3-? (ignored)
     *  combinations = [3, 5, 6]
     *  Available Columns for removal: [0, 1], [0, 2], [1, 2]
     *
     * @param start        The starting column index for combination generation
     * @param current      The current combination represented as a bitmask
     * @param numSelect    The number of columns left to select
     * @param cLen         The total number of columns available
     * @param combinations The list to store all generated combinations
     */
    private void generateCombinations(int start, int current, int numSelect, int cLen, List<Integer> combinations) {
        if (numSelect == 0) {
            combinations.add(current);
            return;
        }
        //A. Delete column 'i'
        for (int i = start; i < cLen; i++) {
            //B. Convert "current+= 1<<i" to "current + (1 << i)" to avoid the affection for the next traversal.
            generateCombinations(i+1, current + (1 << i), numSelect-1, cLen, combinations);
        }
    }

}