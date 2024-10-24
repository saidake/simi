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
 * Initialization:
 *     Start from the first element
 * Optional Conditions:
 *     Mark all negative numbers.
 * ---------------------------------------------------------- Deep-first search
 * 1
 * 0
 * >>>
 * 1 [0]
 * 0 [-2]
 * >>>
 * 1 [0]
 * 0 [-2]
 * 0 3
 * >>>
 * 1 [0]  1
 * 0 [-2] 3
 * 0 3    8
 * >>>
 * 1 [0]  1
 * 0 [-2] 3
 * 0 3    8
 * 0 -2   3
 *
 */
public class MaxSumOfRectangleNoLargerThanK {
    public int maxSumSubmatrix(int[][] matrix, int k) {
        return 0;
    }
}
