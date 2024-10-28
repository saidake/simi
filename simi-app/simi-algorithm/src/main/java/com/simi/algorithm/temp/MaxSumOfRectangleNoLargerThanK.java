package com.simi.algorithm.temp;

import java.util.*;

/**
 * https://leetcode.cn/problems/max-sum-of-rectangle-no-larger-than-k/description/
 * <pre>
 * Max Sum of Rectangle No Larger Than K
 * Constraints:
 *     m == matrix.length
 *     n == matrix[i].length
 *     1 <= m, n <= 100
 *     -100 <= matrix[i][j] <= 100
 *     -105 <= k <= 105
 * ---------------------------------------------------------- Analysis
 * max sum = k
 * m x n =  9 x 6
 * 1   0   1   9  -1   2  -3  -4   3
 * 3  -2   3   8   9  -7   7  -6   2
 * 4 [ 3   8   9  -1]  2   3   1   3
 * 2 [-2   3   8   8] -7  -6   2  -4
 * 5 [ 0   5   9  -1]  4  -5   1   1
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
 *     The diffusion algorithm only need half points as start points in the matrix.
 * ---------------------------------------------------------- Traversal
 * 1   0   1   9  -1   2  -3  -4   3
 * 3  -2   3   8   9  -7   7  -6   2
 * 4   3   8   9  -1   2   3   1   3
 * 2  -2   3   8   8  -7  -6   2  -4
 * 5   0   5   9  -1   4  -5   1   1
 * 3  -9   1  -2   3  -4   9  -6  -8
 * </pre>
 *
 */
public class MaxSumOfRectangleNoLargerThanK {
    // -105 <= k <= 105
    public int maxSumSubmatrix(int[][] matrix, int k) {
        //A. Common data.
        int rLen = matrix.length;
        int cLen = matrix[0].length;
        int max = Integer.MIN_VALUE;
        //A. Traverse column indexes.
        // 1   0   1   9  -1   2  -3  -4   3
        // 1 ->                            3   (Calculate the sum of each row from column rang 1 -> 3)
        //     0 ->                        3
        //         1 ->                    3
        // ...
        for (int start = 0; start < cLen; start++) {
            int[] rowSums = new int[rLen];
            for (int cur = start; cur < cLen; cur++) {
                //B. Update the row sums for the current column range [left, right].
                // 1   0   1   9  -1   2  -3  -4   3        rowSums[0] = 1  -> 1  -> 2
                // ↓   ↓   ↓
                // 3  -2   3                                rowSums[1] = 3  -> 1  -> 4
                // ↓   ↓   ↓
                // 4   3   8                                rowSums[2] = 4  -> 7  -> 15
                // ...
                for (int i = 0; i < rLen; i++) {
                    rowSums[i] += matrix[i][cur];
                }
                max = Math.max(max, getMaxSubarraySumNoLargerThanK(rowSums, k));
                if (max == k) return k;
            }
        }
        return max;
    }

    private int getMaxSubarraySumNoLargerThanK(int[] arr, int k) {
        TreeSet<Integer> prefixSums = new TreeSet<>();
        prefixSums.add(0);
        int currentSum = 0;
        int maxSum = Integer.MIN_VALUE;

        for (int num : arr) {
            currentSum += num;
            // We want to find the smallest prefix sum such that currentSum - prefixSum <= k
            Integer prefix = prefixSums.ceiling(currentSum - k);
            if (prefix != null) {
                maxSum = Math.max(maxSum, currentSum - prefix);
            }
            // Add the current sum to the prefix sums set.
            prefixSums.add(currentSum);
        }
        return maxSum;
    }

}
