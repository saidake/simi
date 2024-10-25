package com.simi.algorithm.temp;

/**
 * https://leetcode.cn/problems/max-sum-of-rectangle-no-larger-than-k/description/
 * Max Sum of Rectangle No Larger Than K
 * <pre>
 * Constraints:
 *     m == matrix.length
 *     n == matrix[i].length
 *     1 <= m, n <= 100
 *     -100 <= matrix[i][j] <= 100
 *     -105 <= k <= 105
 * </pre>
 * ---------------------------------------------------------- Analysis
 * max sum = k
 * m x n =  9 x 6
 * 1   0   1   9  -1   2  -3  -4   3
 * 0  -2   3   8   9  -7   7  -6   2
 * 0 [ 3   8   9  -1]  2   3   1   3
 * 0 [-2   3   8   8] -7  -6   2  -4
 * 0 [ 0   5   9  -1]  4  -5   1   1
 * 3  -9   1  -2   3  -4   9  -6  -8
 * First assume that the target rectangle is the following rectangle:
 * max sum = 19 + 17 + 13 = 49
 *  3  8  9 -1
 * -2  3  8  8
 *  0  5  9 -1
 * Consideration:
 *     The size of the rectangle cannot be determined.
 *     The location of the rectangle cannot be determined.
 * Optional Conditions:
 *     Mark all negative numbers. (invalid)
 *     First calculate the sum of each row and the sum of each column.
 * Initialization:
 *     Start from the first element
 * Core Logic:
 *     Position the rectangle based on the upper left and lower right corner points.
 * ---------------------------------------------------------- Depth-first search
 * 1   0   1   9  -1   2  -3  -4   3
 * 0  -2  [3]  8   9  -7   7  -6   2
 * 0   3   8   9  -1   2   3   1   3
 * 0  -2   3   8   8  -7  -6   2  -4
 * 0   0   5   9  -1   4  -5   1   1
 * 3  -9   1  -2   3  -4   9  -6  -8
 *
 *
 */
public class MaxSumOfRectangleNoLargerThanK {
    public int maxSumSubmatrix(int[][] matrix, int k) {
        //A. Common data.
        int rLen=matrix.length;
        int cLen=matrix[0].length;
        //A. Traverse the matrix based on the upper left and lower right corner points.
        for (int i = 0; i < rLen; i++) {
            for (int j = 0; j < cLen; j++) {
                int start=matrix[i][j];
            }
        }
        return 0;
    }
    private void dfs(int[][] matrix, int i, int j){

    }
}
