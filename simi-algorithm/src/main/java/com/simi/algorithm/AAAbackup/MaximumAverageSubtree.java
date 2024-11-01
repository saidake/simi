package com.simi.algorithm.AAAbackup;

import com.simi.common.test.domain.TreeNode;
import lombok.extern.slf4j.Slf4j;

/**
 * https://leetcode.cn/problems/maximum-average-subtree/description/
 * Maximum Average Subtree
 * <pre>
 * Constraints:
 *      The number of nodes in the tree is in the range [1, 104].
 *      0 <= Node.val <= 105
 * </pre>
 */
@Slf4j
public class MaximumAverageSubtree {
    private double maxAvgSum=0;
    private class BackTrackObj {
        public int sum=0;
        public int nodeNum=0;

        public BackTrackObj(int sum, int nodeNum){
            this.sum=sum;
            this.nodeNum=nodeNum;
        }
    }
    public double maximumAverageSubtree(TreeNode root) {
        postorder(root);
        return this.maxAvgSum;
    }

    /**
     * Traverse binary tree in postorder.
     * Node value range: 0 <= Node.val <= 105
     *                            2
     *                 6                      3
     *            -          5          12           7
     *                    -    -     0     4        -    10
     *                             - -    11   -        -   9
     *                                   - 1               -  8
     * @param root  Root node
     */
    private BackTrackObj postorder(TreeNode root){
        //A. Creating the BackTrackObj to store return values is necessary because there is more than one return value.
        if(root!=null){
            BackTrackObj lo=postorder(root.left);
            BackTrackObj ro=postorder(root.right);
            int nodeNum=0;
            int curSum=0;
            if(root.left!=null&&root.right!=null){
                nodeNum=lo.nodeNum + ro.nodeNum + 1;
                curSum=lo.sum+ro.sum+root.val;
            }
            else if(root.left!=null){
                nodeNum=lo.nodeNum  + 1;
                curSum=lo.sum+root.val;
            }
            else if(root.right!=null) {
                nodeNum=ro.nodeNum  + 1;
                curSum=ro.sum+root.val;
            }
            else {
                nodeNum=1;
                curSum=root.val;
            }
            double curAvg=(double)curSum/nodeNum;
            this.maxAvgSum=Math.max(maxAvgSum,curAvg);
            return new BackTrackObj(curSum, nodeNum);
        }
        return null;
    }
}
