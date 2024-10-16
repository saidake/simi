package com.simi.algorithm.temp;

import java.util.Arrays;
import java.util.Deque;
import java.util.LinkedList;

/**
 * https://leetcode.cn/problems/tallest-billboard/description/
 * <pre>
 * Conditions:
 *     You are given a collection of rods that can be welded together.
 *     Return the largest possible height of your billboard installation.
 * Constraints:
 * 1 <= rods.length <= 20
 * 1 <= rods[i] <= 1000
 * sum(rods[i]) <= 5000
 * </pre>
 */
public class TallestBillboard {
    /**
     * Temporary Solution.
     * <pre>
     * [1,2,3,4,5,6]   2,3,5 = 4,6  ->  10 = 10
     *      1+2+3+4+5+6 = 21
     *      21/2 = 10.5
     * 1,2,3 =  6     -> 6 = 6
     * 1,2,3,4 > 6    -> 10 > 6   ->  10 - 2 = 6 + 2 (valid)
     * 1,2,3,4 < 5,6  -> 10 < 11
     * </pre>
     *
     * @param rods  Available rods.
     * @return  The largest height of the billboard.
     */
    public int tallestBillboard(int[] rods) {
        //A. sort the rods first.
        Arrays.sort(rods);
        int sum = Arrays.stream(rods).sum();
        double halfSum=sum/2.0;
        int len=rods.length;
        //A.define the left result subset and the right result subset.
        //A.[TP] First assume that the tail element must be contained in the right result subset.
        int lSum=0, rSum=0;
        Deque<Integer> lRodList=new LinkedList<>();
        Deque<Integer> rRodList=new LinkedList<>();
        for (int i = 0; i < len; i++) {
            //...
            lSum+=rods[i];
            rSum+=rods[len-1-i];
        }
        return 0;
    }
}
