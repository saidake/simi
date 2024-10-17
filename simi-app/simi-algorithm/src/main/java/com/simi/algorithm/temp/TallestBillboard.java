package com.simi.algorithm.temp;

import java.util.*;

/**
 * https://leetcode.cn/problems/tallest-billboard/description/
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
            if(lSum<=halfSum){
                lRodList.offerLast(rods[i]);
                lSum+=rods[i];
            }
            //B. break if lSum plus rSum equals the sum.
            if(rSum< halfSum-lSum){
                rRodList.offerLast(rods[len-1-i]);
                rSum+=rods[len-1-i];
            }else break;
        }
        //A. start checking the number that can be moved.
        //A.[TEMP] Only simple checks are performed first.
        if(lSum<rSum){
            //B. 1,2,3,4 < 5,6  (10 < 11)   valid moving digit = 0.5
            // [TEMP] First decimals are ignored and no rounding is performed.
            int validMovingDigit=(rSum-lSum)/2;
            ListIterator<Integer> rListIterator = rRodList.listIterator(rRodList.size());
            Integer movingNumber;
            while (rListIterator.hasPrevious()){
                int rP=rListIterator.previous();
                if(rP<=validMovingDigit){
                    rListIterator.remove();
                    movingNumber=rP;
                    break;
                }
            }
//            insertAndKeepSorted(lRodList,)

        }
        return 0;
    }

    public static void insertAndKeepSorted(AbstractList<Integer> sortedList, int newNumber) {
        ListIterator<Integer> iterator = sortedList.listIterator();

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
