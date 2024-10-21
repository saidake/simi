package com.simi.algorithm.dynamicprogramming;

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

    /**
     * Temporary Solution.
     * <pre>
     * [1,2,3,4,5,6]   2,3,5 = 4,6  ->  10 = 10
     *     1+2+3+4+5+6 = 21
     *     21/2 = 10.5
     * 1,2,3,4 < 5,6  (10 < 11)   valid moving digit = 0.5
     * Removing any number from the right array will not make the sums of both sides equal, so move the smaller number to the left array.
     *     1,2,3,4,5 > 6 (15 > 6)  valid moving digit  = 4.5
     *     Find a number that is most closest to the valid moving digit in the left array from right to left, and move it to the right array.
     *         1,2,3,5 = 4,6 ( 11 > 10)  valid moving digit  = 0.5
     *         There exists a number that can be deleted so that sums of both sides are equal.
     * </pre>
     *
     * @param rods  Available rods.
     * @return  The largest height of the billboard.
     */
    public int tallestBillboard(int[] rods) {
        //A. sort the rods first.
        Arrays.sort(rods);
        int sum = Arrays.stream(rods).sum();
        int halfSum=sum/2;
        int len=rods.length;
        //A.define the left result subset and the right result subset.
        int lSum=0, rSum=0;
        LinkedList<Integer> lRodList=new LinkedList<>();
        LinkedList<Integer> rRodList=new LinkedList<>();
        //A. traverse rods array from both ends simultaneously.
        for (int i = 0; i < len; i++) {
            if(lSum<halfSum){
                lRodList.offerLast(rods[i]);
                lSum+=rods[i];
            }
            //B. break if lSum plus rSum equals the sum.
            if(rSum< halfSum-lSum){
                rRodList.offerLast(rods[len-1-i]);
                rSum+=rods[len-1-i];
            }
        }
        //A. start checking the number that can be moved.
        //A.[TEMP] Only simple checks are performed first.
        while (lSum!=rSum){
            //B. 1,2,3,4 < 5,6  (10 < 11)   valid moving digit = 0.5
            // [TEMP] First decimals are ignored and no rounding is performed.
            int validMovingDigit=Math.abs(rSum-lSum)/2;
            LinkedList<Integer> sourceRodList=rSum>lSum?rRodList:lRodList;
            LinkedList<Integer> targetRodList=rSum>lSum?lRodList:rRodList;
            int sourceSum=rSum>lSum?rSum:lSum;
            int targetSum=rSum>lSum?lSum:rSum;
            //B. Find a number that can be deleted so that the lSum equals to rSum.
            Iterator<Integer> iterator = sourceRodList.iterator();
            boolean deletedNumber=false;
            while (iterator.hasNext()){
                Integer next = iterator.next();
                if(sourceSum-next==targetSum){
                    iterator.remove();
                    if(sourceRodList==rRodList)rSum-=next;
                    else lSum-=next;
                    deletedNumber=true;
                }
            }
            if(deletedNumber)continue;
            //B. Find the closest number to the validMovingDigit and move it.
            ListIterator<Integer> moveListIterator = sourceRodList.listIterator(sourceRodList.size());
            Integer movingNumber = null;
            while (moveListIterator.hasPrevious()){
                int rP=moveListIterator.previous();
                if(rP<=validMovingDigit){
                    moveListIterator.remove();
                    movingNumber=rP;
                    break;
                }
            }
            if(rSum>lSum){
                rSum-=movingNumber;
                lSum+=movingNumber;
            } else {
                lSum-=movingNumber;
                rSum+=movingNumber;
            }
            insertAndKeepSorted(targetRodList, movingNumber);
        }
        return lSum;
        // B.  1,2,3,4,5 > 6 (15 > 6)  valid moving digit  = 4.5
    }

    /**
     * Insert a number to a sorted list before the first number that is greater than it,
     *
     * @param sortedList    A sorted list.
     * @param newNumber     The number to be inserted, can be null.
     */
    public static void insertAndKeepSorted(AbstractList<Integer> sortedList, Integer newNumber) {
        ListIterator<Integer> iterator = sortedList.listIterator();
        //A. Check if the new number is less than or equal to the first element
        if (!iterator.hasNext() || iterator.next() > newNumber) {
            //B. Insert at the beginning
            iterator.add(newNumber);
            return;
        }
        //A. Find the correct position to insert the new number
        while (iterator.hasNext()) {
            if (iterator.next() > newNumber) {
                //B. Move back one position and insert the new number
                iterator.previous();
                iterator.add(newNumber);
                return;
            }
        }
        //A. If we reached the end, add the new number at the end
        sortedList.add(newNumber);
    }
}
