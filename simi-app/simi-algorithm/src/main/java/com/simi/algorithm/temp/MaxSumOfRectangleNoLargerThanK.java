package com.simi.algorithm.temp;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * https://leetcode.cn/problems/max-sum-of-rectangle-no-larger-than-k/description/
 * <pre>
 * Max Sum of Rectangle No Larger Than K
 * Constraints:
 *     m == matrix.length
 *     n == matrix[i].length
 *     1 <= m, n <= 100
 *     -100 <= matrix[i][j] <= 100
 *     -105 <= k <= 105
 * ---------------------------------------------------------- Analysis
 * max sum = k
 * m x n =  9 x 6
 * 1   0   1   9  -1   2  -3  -4   3
 * 0  -2   3   8   9  -7   7  -6   2
 * 0 [ 3   8   9  -1]  2   3   1   3
 * 0 [-2   3   8   8] -7  -6   2  -4
 * 0 [ 0   5   9  -1]  4  -5   1   1
 * 3  -9   1  -2   3  -4   9  -6  -8
 * First assume that the target rectangle is the following rectangle:
 * max sum = 19 + 17 + 13 = 49
 *  3  8  9 -1
 * -2  3  8  8
 *  0  5  9 -1
 * Consideration:
 *     The size of the rectangle cannot be determined.
 *     The location of the rectangle cannot be determined.
 * Optional Conditions:
 *     Mark all negative numbers. (invalid)
 *     First calculate the sum of each row and the sum of each column.
 * Initialization:
 *     Start from the first element
 * Core Logic:
 *     Position the rectangle based on the upper left and lower right corner points.
 *     The diffusion algorithm only need half points as start points in the matrix.
 * ---------------------------------------------------------- Traversal
 * Start point is [8]
 * 1   0   1   9  -1   2  -3  -4   3
 * 0  -2   3   8   9  -7   7  -6   2
 * 0   3   8   9  -1   2   3   1   3
 * 0  -2   3  [8]  8  -7  -6   2  -4
 * 0   0   5   9  -1   4  -5   1   1
 * 3  -9   1  -2   3  -4   9  -6  -8
 * >>>
 * Look around for the end point,
 * The end point is picked from the square:
 *     [8] -> 9 -> [-1] -> 8->  [-1] -> 9 -> [5] -> 3
 * Sum: 8+3, (8+3)+(8+9), 8+9, (8+9)+8-1, 8+8, 8-1+(8+9), 8+9, (8+9)+5+3
 * 1   0   1   9  -1   2  -3  -4   3
 * 0  -2   3   8   9  -7   7  -6   2
 * 0   3  [8]  9 [-1]  2   3   1   3
 * 0  -2   3  [8]  8  -7  -6   2  -4
 * 0   0  [5]  9 [-1]  4  -5   1   1
 * 3  -9   1  -2   3  -4   9  -6  -8
 * >>>
 * Expand the chosen square by 1.
 * Current Square: [-2] -> [-7] -> [-4] -> [-9]
 * 1   0   1   9  -1   2  -3  -4   3
 * 0 [-2]  3   8   9 [-7]  7  -6   2
 * 0   3  [8]  9 [-1]  2   3   1   3
 * 0  -2   3  [8]  8  -7  -6   2  -4
 * 0   0  [5]  9 [-1]  4  -5   1   1
 * 3 [-9]  1  -2   3 [-4]  9  -6  -8
 * >>
 * Ignore the overflow part and turn the square into a rectangle.
 *[1]  0   1   9  -1   2 [-3] -4   3
 * 0 [-2]  3   8   9 [-7]  7  -6   2
 * 0   3  [8]  9 [-1]  2   3   1   3
 * 0  -2   3  [8]  8  -7  -6   2  -4
 * 0   0  [5]  9 [-1]  4  -5   1   1
 * 3 [-9]  1  -2   3 [-4]  9  -6  -8
 * </pre>
 *
 */
public class MaxSumOfRectangleNoLargerThanK {
    /**
     * Store the addends in each direction.
     * 0 left, 1 top, 2 right, 3 bottom, 4 upper left corner, 5 upper right corner, 6 lower right corner, 7 lower left corner.
     */
    private Map<Integer, List<Integer>> directionMap=new HashMap<>();
    {
        directionMap.put(0, Arrays.asList(0,-1));
        directionMap.put(1, Arrays.asList(1,0));
        directionMap.put(2, Arrays.asList(0,1));
        directionMap.put(3, Arrays.asList(-1,0));
        directionMap.put(4, Arrays.asList(1,-1));
        directionMap.put(5, Arrays.asList(1,1));
        directionMap.put(6, Arrays.asList(-1,1));
        directionMap.put(7, Arrays.asList(-1,-1));

    }
    public int maxSumSubmatrix(int[][] matrix, int k) {
        //A. Common data.
        int rLen = matrix.length;
        int cLen = matrix[0].length;
        int max = 0;
        //A. Traverse the matrix based on the upper left and lower right corner points.
        for (int i = 0; i < rLen; i++) {
            for (int j = 0; j < cLen; j++) {
                //B. Only half points are needed.
                int start = matrix[i][j];
                int area = start;
                //B. First calculate area from left, right, top, bottom.
                max=Math.max(max,this.calculateArea(matrix, area, k, 0, i, j, i, j));
            }
        }
        return max;
    }

    /**
     * Calculate the area of a matrix based on start point and end point.
     * @param matrix    Original matrix.
     * @param area      Area of the current rectangle.
     * @param i         Row index of start point.
     * @param j         Column index of start point.
     * @param m         Row index of end point.
     * @param n         Column index of start point.
     */
    private int calculateArea(int[][] matrix, int area, int k, int direction, int i, int j, int m, int n){
        if(m<0 || m>=matrix.length||n<0||n>=matrix[0].length)return area;
        int newArea=0;
        switch (direction){
            case 0:
            case 1:
            case 2:
            case 3:
                newArea = area + matrix[m][n];
        }
        if(newArea==k)return newArea;
        List<Integer> dm = directionMap.get(direction);
        return calculateArea(matrix,newArea>k?area:Math.max(newArea,area), k, direction,i,j,m+dm.get(0), n+dm.get(1));
    }

}
