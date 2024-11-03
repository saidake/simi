package com.simi.algorithm.AAAbackup;

/**
 * <pre>
 * Constraints:
 *     1 <= nums.length <= 20
 *     0 <= nums[i] <= 1000
 *     0 <= sum(nums[i]) <= 1000
 *     -1000 <= target <= 1000
 * </pre>
 */
public class TargetSum2 {
    /**
     * Standard Solution
     *
     * @param nums  Source array
     * @param target The target value
     * @return The maximum numbers of valid expressions in the array nums.
     */
    public int findTargetSumWays(int[] nums, int target) {
        // Calculate the sum of nums.
        int sum = 0;
        for (int num : nums) {
            sum += num;
        }
        // Check whether the sum can be greater than the target.
        int diff = sum - target;
        if (diff < 0 || diff % 2 != 0) {
            return 0;
        }
        int neg = diff / 2;
        // Calculate the sum of all negative numbers in the array nums (0 <= sum(nums[i]) <= 1000).
        return dfs(nums,neg ,nums.length-1);
    }

    private int dfs(int[] nums, int neg, int ind){
        // End condition
        if(neg<0)return 0;
        if(ind==0){
            if(neg==nums[ind]&&neg==0)return 2;
            else if(neg==nums[ind])return 1;
            else if(neg==0)return 1;
            else return 0;
        }
        return dfs(nums,neg,ind-1) + dfs(nums, neg-nums[ind],ind-1);
    }
}
