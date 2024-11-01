package com.simi.algorithm.temp;

import java.util.*;

/**
 * <a href="https://leetcode.cn/problems/tallest-billboard/description/">Tallest Billboard</a>
 * <pre>
 * Conditions:
 *     You are given a collection of rods that can be welded together.
 *     Return the largest possible height of your billboard installation.
 * Constraints:
 *     1 <= rods.length <= 20
 *     1 <= rods[i] <= 1000
 *     sum(rods[i]) <= 5000
 * </pre>
 */
public class TallestBillboard {

    /**
     * DP solution for "TallestBillboard".
     * <pre>
     * rods = [1,2,3,4,5,6]
     * map[ diff - maxSum ]
     * key = diff + rod or diff - rod,  value = Math.max( map[key], (maxSum + rod) or maxSum)
     *
     * dp = {0: 0}
     *      This represents the starting state where the difference (key) is 0,
     *      and the maximum sum possible is also 0.
     * dp = {0: 0, 1: 0}
     *      (rod 1)
     *      new keys: 1
     *      After processing the first rod, a new difference of 1 can be
     *      reached with sum 0.
     * dp = {0: 0, 1: 0, 2: 0, 3: 0}
     *      (rod 2)
     *      new keys: 2, 3
     *      Adding this rod creates new states (difference 2 and 3).
     * dp = {0: 0, 1: 0, 2: 0, 3: 0, 4: 0, 6: 3}
     *      (rod 3)
     *      new keys: 4, 6
     *      new values: 3
     *      New states arise from adding the rod to one side or removing it
     *      from the other.s
     * dp = {0: 0, 1: 0, 2: 0, 3: 0, 4: 0, 5: 0, 6: 3, 8: 3}
     *      (rod 4)
     *      new keys: 5, 8
     * dp = {0: 0, 1: 0, 2: 0, 3: 0, 4: 0, 5: 0, 6: 3, 7: 0, 8: 3, 9: 0, 10: 5}
     *      (rod 5)
     *      new keys: 7, 9, 10
     * dp = {0: 0, 1: 0, 2: 0, 3: 0, 4: 0, 5: 0, 6: 3, 7: 0, 8: 3, 9: 0, 10: 5, 11: 0, 12: 6}
     *      (rod 6)
     *      new keys: 11, 12
     *
     * </pre>
     *
     * @param rods Rod list.
     * @return  The largest height.
     */
    public int tallestBillboardPassedSolution(int[] rods) {
        //A. Initialize the DP map with the base state (0, 0)
        Map<Integer, Integer> dp = new HashMap<>();
        dp.put(0, 0);
        //A. Iterate through each rod and update the DP map
        for (int rod : rods) {
            //B. Take a snapshot of the current state to avoid concurrent modification
            Map<Integer, Integer> currentDp = new HashMap<>(dp);
            for (Map.Entry<Integer, Integer> entry : currentDp.entrySet()) {
                int diff = entry.getKey();
                int maxSum = entry.getValue();

                //C. Add the rod to the left side (increase the difference)
                int newDiff = diff + rod;
                int newSum = maxSum + rod;
                dp.put(newDiff, Math.max(dp.getOrDefault(newDiff, 0), newSum));

                //C. Add the rod to the right side (decrease the difference)
                newDiff = diff - rod;
                dp.put(newDiff, Math.max(dp.getOrDefault(newDiff, 0), maxSum));
            }
        }

        //A. The maximum sum for diff = 0 is the result
        return dp.getOrDefault(0, 0);
    }
}
