package com.simi.algorithm.math;

import java.util.Arrays;

/**
 * https://leetcode.cn/problems/reduction-operations-to-make-the-array-elements-equal/description/
 * Reduction Operations to Make the Array Elements Equal
 * <pre>
 * Constraints:
 *     1 <= nums.length <= 5 * 104
 *     1 <= nums[i] <= 5 * 104
 * </pre>
 * Example:
 * <pre>
 * nums = [9,10,12,3,4,5,9,10,11]
 * [3,4,5,9,9,10,10,11,12]    - 12 (x1)  ->  1 + 2 + [4] + [6] + 7 + 8
 *  8 7 6   4    2  1
 * [3,4,5,9,9,10,10,11,11]    - 11 (x2)
 * [3,4,5,9,9,10,10,10,11]    - 11
 * [3,4,5,9,9,10,10,10,10]    - 10 (x4)
 * [3,4,5,9,9,9,10,10,10]     - 10
 * [3,4,5,9,9,9,9,10,10]      - 10
 * [3,4,5,9,9,9,9,9,10]       - 10
 * [3,4,5,9,9,9,9,9,9]        - 9 (x6)
 * ...
 * [3,4,5,5,5,5,5,5,5]        - 5 (x7)
 * ...
 * [3,4,4,4,4,4,4,4,4]        - 4 (x8)
 * [3,3,3,3,3,3,3,3,3]        Completed
 * </pre>
 */
public class ReductionOperationsToMakeTheArrayElementsEqual {
    public int reductionOperations(int[] nums) {
        //A. Sort the nums array.
        Arrays.sort(nums);
        //A. Traverse nums array.
        int sum=0;
        int pre=nums[nums.length-1];
        for (int i = nums.length-1-1,j=1; i >=0; pre=nums[i],i--,j++) {
            if(nums[i]!=pre){
                sum+=j;
            }
        }
        return sum;
    }
}
