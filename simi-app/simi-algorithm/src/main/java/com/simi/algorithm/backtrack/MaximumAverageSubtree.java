package com.simi.algorithm.backtrack;

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
    private BackTrackObj postorder(TreeNode root) {
        // Base case: if the node is null, return a zeroed BackTrackObj
        if (root == null) return new BackTrackObj(0, 0);

        // Postorder traversal
        BackTrackObj left = postorder(root.left);
        BackTrackObj right = postorder(root.right);

        // Calculate current sum and node count including the root
        int nodeNum = left.nodeNum + right.nodeNum + 1;
        int curSum = left.sum + right.sum + root.val;

        // Calculate current average and update max average if needed
        double curAvg = (double) curSum / nodeNum;
        maxAvgSum = Math.max(maxAvgSum, curAvg);

        // Return the cumulative sum and node count for the current subtree
        return new BackTrackObj(curSum, nodeNum);
    }
}
