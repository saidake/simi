package com.simi.algorithm.AAAbackup;

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
    private int targetSum=0;

    public int findTargetSumWays(int[] nums, int target) {
        this.depthFirstSearch(nums, target, 0, 0);
        return this.targetSum;
    }

    /**
     * Find a valid path in which the sum of numbers in the array nums equals the target value.
     *
     * @param nums     Source array
     * @param target   Target sum
     * @param index    Current index
     */
    public void depthFirstSearch(int[] nums, int target, int index, int sum){
        if(index>=nums.length) {
            if(sum==target)this.targetSum++;
            return;
        }
        depthFirstSearch(nums, target, index+1, sum+nums[index]);
        depthFirstSearch(nums, target, index+1, sum-nums[index]);
    }
}
