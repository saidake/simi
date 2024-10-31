package com.simi.algorithm.temp;

/**
 * Target Sum
 * https://leetcode.cn/problems/target-sum/description/
 * <pre>
 * Constraints:
 *     1 <= nums.length <= 20
 *     0 <= nums[i] <= 1000
 *     0 <= sum(nums[i]) <= 1000
 *     -1000 <= target <= 1000
 * </pre>
 *
 */
public class TargetSum {
    /**
     * Define a two-dimensional array dp, where dp[i][j] represents the number
     * of solutions to select elements from the first i numbers of the array nums
     * so that the sum of these elements is equal to j.
     * dp[i][j]=
     *      dp[i−1][j]                          j < nums[i]
     *      dp[i−1][j] + dp[i−1][j−nums[i]]     j >= nums[i]
     *
     * @param nums
     * @param target
     * @return
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
        //A. Calculate the sum of all negative numbers in the array nums.
        // 7 9 8 3 4 5 4 1 9 2 8 7   sum = 67
        // target =  67 + (- 8 - 4 -5 - 1 - 2 - 7)*2 = 13
        // neg = (67-13)/2 = 27
        // dp[0]=1
        // >>>
        // num = 7, j = 27 -> 7
        //     dp[27] = dp[27] + dp[20] = 0
        //     ...
        //     dp[7] = dp[7] + dp[0] = 1 (dp[7]=1)
        //>>>
        // num = 9, j = 27 -> 9
        //     dp[27] = dp[27] + dp[16] = 0
        //     ...
        //     dp[16] = dp[16] + dp[7] = 1 (dp[16]=1)
        //     dp[9] = dp[9] + dp[0] = 1 (dp[9]=1)
        // >>>
        // num = 8, j = 27 -> 8
        //     dp[27] = dp[27] + dp[19] = 0
        //     ...
        //     dp[24] = dp[24] + dp[16] = 1 (dp[24]=1)
        //     dp[17] = dp[17] + dp[9] = 1 (dp[17]=1)
        //     dp[15] = dp[15] + dp[7] = 1 (dp[15]=1)
        //     dp[8] = dp[8] + dp[0] = 1 (dp[8]=1)
        // >>
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
