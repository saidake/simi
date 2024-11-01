package com.simi.algorithm.dynamicprogramming;

public class TargetSum {
    /**
     * Standard Solution
     *
     * @param nums  Source array
     * @param target The target value
     * @return The maximum numbers of valid expressions in the array nums.
     */
    public int findTargetSumWays(int[] nums, int target) {
        //A. Calculate the sum of nums.
        int sum = 0;
        for (int num : nums) {
            sum += num;
        }
        //A. Check whether the sum can be greater than the target.
        int diff = sum - target;
        if (diff < 0 || diff % 2 != 0) {
            return 0;
        }
        // A. Calculate the sum of all negative numbers in the array nums.
        int neg = diff / 2;
        int[] dp = new int[neg + 1];
        dp[0] = 1;
        for (int num : nums) {
            for (int j = neg; j >= num; j--) {
                dp[j] += dp[j - num];
            }
        }
        return dp[neg];
    }
}
