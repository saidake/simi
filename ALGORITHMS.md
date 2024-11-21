# Table of Contents
[Back to Main Project README](README.md)
- [Array](#array)
    - [Array Partition](#array-partition)
- [Dynamic Programming](#dynamic-programming)
    - [Decode Ways II](#decode-ways-ii)
    - [Stone Game](#stone-game)
    - [Target Sum](#target-sum)
- [Sliding Window](#sliding-window)
    - [Find the Longest Equal Subarray](#find-the-longest-equal-subarray)
- [String](#string)
    - [License Key Formatting](#license-key-formatting)
- [Uncategorized Problems](#uncategorized-problems)
    - [Maximize Value of Function in a Ball Passing Game](#maximize-value-of-function-in-a-ball-passing-game)
    - [Find Number of Ways to Reach the K-th Stair](#find-number-of-ways-to-reach-the-k-th-stair)
    - [Climbing Stairs](#climbing-stairs)
# Array
## Array Partition
[Back to Top](#table-of-contents)  
### Overview
Given an integer array `nums` of 2n integers, 
group these integers into `n` pairs `(a1, b1), (a2, b2), ..., (an, bn)` such that the sum of `min(ai, bi)` for all `i` is maximized. 
Return the maximized sum.

**Example 1:**
> Input: nums = [1,4,3,2]  
> Output: 4  
> Explanation: All possible pairings (ignoring the ordering of elements) are:
> 1. (1, 4), (2, 3) -> min(1, 4) + min(2, 3) = 1 + 2 = 3
> 2. (1, 3), (2, 4) -> min(1, 3) + min(2, 4) = 1 + 2 = 3
> 3. (1, 2), (3, 4) -> min(1, 2) + min(3, 4) = 1 + 3 = 4
> 
> So the maximum possible sum is 4.

**Example 2:**
> Input: nums = [6,2,6,5,1,2]  
> Output: 9  
> Explanation:   
> The optimal pairing is (2, 1), (2, 5), (6, 6). min(2, 1) + min(2, 5) + min(6, 6) = 1 + 2 + 6 = 9.

**Constraints:**
* 1 <= n <= 104
* nums.length == 2 * n
* -104 <= nums[i] <= 104
### Analysis
In each group, the larger integer will be omitted, and we need to maximize the `sum`.
Therefore, The omitted value must be smaller.
To ensure this, wen can sort the array, so that the smaller integer is omitted when calculating the minimal value from the group.

#### Implementation
```java
class Solution {
    public int arrayPairSum(int[] nums) {
        Arrays.sort(nums);
        int sum=0;
        for(int i=0; i< nums.length; i+=2){
            sum+=nums[i];
        }
        return sum;
    }
}
```
##### Time and Space Complexity
* Time Complexity: $ O(n \log n) $

    The time complexity of `Arrays.sort(nums)` is $ O(n \log n) $.  
    The loop iterates through the array with a step of 2, so it runs $ 𝑛/2 $ times. 
    The time complexity of this loop is $ O(n) $.  
    The sorting step dominates the iteration step. 
    Hence, The total time complexity is $ O(n \log n) $.
* Space Complexity: $ O(1) $

    The Arrays.sort() method uses $ O(1) $ space for primitive data types like integers in Java, as it utilizes a variation of quicksort (dual-pivot quicksort).
    There is no additional space used apart from the input array and a few variables.   
    Therefore, the total space complexity is $ O(1) $.
# Dynamic Programming
## Decode Ways II
[Back to Top](#table-of-contents)  
### Overview
A message containing letters from `A-Z` can be encoded into numbers using the following mapping:
```text
'A' -> "1"
'B' -> "2"
...
'Z' -> "26"
```
To decode an encoded message, all the digits must be grouped then mapped back into letters using the reverse of the mapping above (there may be multiple ways). For example, "11106" can be mapped into:

`"AAJF"` with the grouping `(1 1 10 6)`
`"KJF"` with the grouping `(11 10 6)`
Note that the grouping `(1 11 06)` is invalid because `"06"` cannot be mapped into `'F'` since `"6"` is different from `"06"`.

In addition to the mapping above, an encoded message may contain the `'*'` character, which can represent any digit from '1' to '9' ('0' is excluded). For example, the encoded message "1*" may represent any of the encoded messages `"11"`, `"12"`, `"13"`, `"14"`, `"15"`, `"16"`, `"17"`, `"18"`, or `"19"`. Decoding `"1*"` is equivalent to decoding any of the encoded messages it can represent.

Given a string `s` consisting of digits and `'*'` characters, return the **number** of ways to **decode** it.

Since the answer may be very large, return it **modulo** $ 10^9+7 $.

**Example 1:**
> Input: s = "*"  
> Output: 9  
> Explanation: The encoded message can represent any of the encoded messages "1", "2", "3","4", "5", "6", "7", "8", or "9".  
> Each of these can be decoded to the strings "A", "B", "C", "D", "E", "F", "G", "H", and"I" respectively.  
> Hence, there are a total of 9 ways to decode "*".  

**Example 2:**
> Input: s = "1*"  
> Output: 18  
> Explanation: The encoded message can represent any of the encoded messages "11", "12", "13", "14", "15", "16", "17", "18", or "19".  
> Each of these encoded messages have 2 ways to be decoded (e.g. "11" can be decoded to "AA" or "K").  
> Hence, there are a total of 9 * 2 = 18 ways to decode "1*".

**Example 3:**
> Input: s = "2*"  
> Output: 15  
> Explanation:  
> The encoded message can represent any of the encoded messages "21", "22","23", "24", "25", "26", "27", "28", or "29".  
> "21", "22", "23", "24", "25", and "26" have 2 ways of being decoded, but "27", "28", and"29" only have 1 way.  
> Hence, there are a total of (6 * 2) + (3 * 1) = 12 + 3 = 15 ways to decode "2*".

Constraints:
* 1 <= s.length <= 105
* s[i] is a digit or '*'.
### Analysis
Here is a random example to demonstrate the decode process:
```text
indices: 0 1 2 3 4 5 6 7   
s:       3 2 9 * 8 1 4 2
```
First, The numbers `7, 8, 9` can only be decoded as a single number, 
and the numbers `1-6` can be decoded as a single number or when the previous number is `1` or `2`,
they can be decoded together with the previous number as a new number.
#### Dynamic Programming Solution
When determining the number of ways to decode a sequence, we can break it down into two distinct parts for analysis.  

Here is an simple example:
```text
2392576 
```
Divide the number into `2392` and `576`, so the total number of decoding ways is the product of the decoding ways for `2392` and `576`.  

However, There is another case we need to consider:  

When the digit `2` on the left and the digit `5` on the right are decoded as a pair, 
the product of decoding ways for `2392` and `576` alone is insufficient to account for all possible decoding methods.  
Thus, the total number of decoding ways should also **include** the product of the decoding ways for `239` and `25` and `76`.

The DP process below traverses by index, checking one number at a time.  
For example, the process will consider `2392` and `5`. 
The total number of decoding ways should be the sum of the product of decoding ways for `239` and `5`, plus the product of decoding ways for `239` and `25`.

##### Initialization 
Define a one-dimensional array `dp`, where `dp[i]` represents the number of ways to decode the  the string `s` from index `0` to index `i-1`.
The length of the `dp` array is `s.length+1`, providing space to prevent missing the value of `dp[i-2]`.
#####  Filling the DP Table  
The dynamic programming equation can be expressed as follows:

Single-digit decoding (current character only):
```text
dp[i] += 
    dp[i-1],    s[i] = 1-9
    dp[i-1]×9,  s[i] = * 
```
Two-digit decoding (current and previous character together):
```text
dp[i] += 
    dp[i-2],    s[i] = 7~9 and s[i-1]=1 or *
    0,          s[i] = 7~9 and s[i-1] = 3~9 or 0 or 2
    dp[i-2],    s[i] = 0~6 and s[i-1] = 1 or 2
    dp[i-2]×2,  s[i] = 0~6 and s[i-1] = *
    0,          s[i] = 0~6 and s[i-1] = 3~9 or 0
    dp[i-2]×9,  s[i] = * and s[i-1] = 1 
    dp[i-2]×6,  s[i] = * and s[i-1] = 2
    dp[1-2]*15, s[i] = * and s[i-1] = *
    0,          s[i] = * and s[i-1] = 3~9 or 0
```
##### Result
The `dp[len]` is the number of ways to decode the string `s`.

##### Implementation
```java
class Solution {
    static final int MOD = 1000000007;
    public int numDecodings(String s) {
        int len=s.length();
        long[] dp=new long[len+1];
        dp[0]=1;
        // Initialize dp array
        dp[1] = (s.charAt(0) == '*') ? 9 : (s.charAt(0) != '0' ? 1 : 0);
        // Traverse array chars
        // indices:  0 1 2 3 4 5 6
        // dp:     0 1 2 3 4 5 6 7
        for(int i=2; i<len+1; i++){
            // Check the string s starting from index 1
            // Use characters instead of numeric value to prevent confusion
            char val=s.charAt(i-1);
            char pre=s.charAt(i-2);
            // Single-digit decoding (current character only)
            // '0' has no valid encoding as a single digit
            if (val >= '1' && val <= '9') {
                dp[i] += dp[i-1];
            } else if (val == '*') {
                dp[i] += dp[i-1] * 9;
                if(dp[i]>MOD)dp[i]%=MOD;
            }
            // Two-digit decoding (current and previous character together)
            if(val>='7'&&(pre=='1'|| pre=='*')){
                dp[i]+=dp[i-2];
            }else if(val>='0'&&val<='6'){
                if(pre=='1'||pre=='2'){
                    dp[i]+=dp[i-2];
                }else if(pre=='*'){
                    dp[i]+=dp[i-2]*2;
                }
            }else if(val=='*'){
                if(pre=='1'){
                    dp[i]+=dp[i-2]*9;
                }else if(pre=='2'){
                    dp[i]+=dp[i-2]*6;
                }else if(pre=='*'){
                    dp[i]+=dp[i-2]*15;
                }
            }
            if(dp[i]>MOD)dp[i]%=MOD;
        }
        return (int)dp[len];
    }
}
```
##### Time and Space Complexity
* Time Complexity: $ O(n) $

    The main loop runs from `𝑖=2` to `𝑖=𝑙𝑒𝑛`, where len is the length of the string `s`.
* Space Complexity: $ O(n) $

    The dp array is of size `𝑛+1`, where `𝑛` is the length of the string.
##### Consideration
* Calculate the single-digit decoding cases first to avoid redundant calculations.
* The time complexity of `s.toCharArray()` is O(n), while the `s.charAt()` has a time complexity of O(1), making `charAt()` more efficient.
* The values in the `dp` array are taken modulo 10<sup>9</sup> + 7, which is still a very large value. long type is required to prevent integer overflow.
* Using `Character.getNumericValue()` to obtain the nummeric value of `'*'` in string `s` will return `'-1'` and using the `-1` for checking purposes can lead to misleading readability.

## Stone Game
[Back to Top](#table-of-contents)
### Overview
Alice and Bob play a game with piles of stones.
There are an even number of piles arranged in a row, and each pile has a positive integer number of stones `piles[i]`.

The objective of the game is to end with the most stones. The **total** number of stones across all the piles is **odd**,
so there are no ties.

Alice and Bob take turns, with Alice starting first. Each turn, a player takes the entire pile of stones either
from the beginning or from the end of the row. This continues until there are no more piles left,
at which point the person with the most stones wins.

Assuming Alice and Bob play optimally, return true if Alice wins the game, or false if Bob wins.

**Example 1:**
> Input: piles = [5,3,4,5]  
> Output: true  
> Explanation:  
> Alice starts first, and can only take the first 5 or the last 5.  
> Say she takes the first 5, so that the row becomes [3, 4, 5].  
> If Bob takes 3, then the board is [4, 5], and Alice takes 5 to win with 10 points.  
> If Bob takes the last 5, then the board is [3, 4], and Alice takes 4 to win with 9 points.  
> This demonstrated that taking the first 5 was a winning move for Alice, so we return true.

**Example 2:** 
> Input: piles = [3,7,2,3]  
> Output: true

**Constraints:**
* 2 <= piles.length <= 500
* piles.length is even.
* 1 <= piles[i] <= 500
* sum(piles[i]) is odd.
### Analysis
#### Depth-first Search Solution
Recursively evaluate the `piles` array from both the start and end,
using a flag variable `isAliceTurn` to track whose turn it is and only calculate the sum for Alice.  
Since the game ends when one player takes more than half of the total stones,
the recursion can terminate early, ensuring only one player wins.
```java
class Solution {
  public boolean stoneGame(int[] piles) {
    int sum = Arrays.stream(piles).sum();
    return dfs(piles, (double)sum/2, 0, piles.length-1,0,true);
  }

  /**
   * Recursive DFS function to determine if Alice can win.
   * 
   * @param piles           The array of stone piles.
   * @param hsum            Half of the total sum of stones (the target for Alice to win).
   * @param left            The current left index of the pile range.    
   * @param right           The current right index of the pile range.
   * @param aliceSum        The current sum of stones collected by Alice.
   * @param isAliceTurn     A boolean indicating whether it is Alice's turn.
   * @return                True if Alice can win, otherwise false.
   */
  private boolean dfs(int[] piles, double hsum, int left, int right, int aliceSum, boolean isAliceTurn){
    // If Alice's current sum exceeds half of the total sum, she wins.
    if(aliceSum>hsum)return true;
    if(left>=piles.length || right<0 || left >= right )return false;
    if(piles[left]>piles[right]){
      return dfs(piles, hsum, left+1, right, aliceSum+piles[left], !isAliceTurn);
    }else if(piles[left]<piles[right]){
      return dfs(piles, hsum, left, right-1, aliceSum+piles[right], !isAliceTurn);
    }else{
      return dfs(piles, hsum, left+1, right, aliceSum+piles[left], !isAliceTurn) || dfs(piles, hsum, left, right-1, aliceSum+piles[right], !isAliceTurn);
    }
  }
}
```
Time and Space Complexity
* Time Complexity: *O*(2<sup>n</sup>)
* Space Complexity: $ O(n) $ (for the recursion stack)
#### Dynamic Programming Solution
##### Initialization  
Define a two-dimensional array `dp`, where both the rows and columns correspond to the indices of the array `piles`.  
The `dp[i][j]` represents the difference between the number of stones Alice has and the number of stones Bob has, when considering the subarray from index `i` to `j` of the piles array.  
When `i` equals `j`, there is only a single pile of stones, which is `piles[i]`.
Since Alice goes first, she takes this pile, so `dp[i][j]=piles[i]`.
#####  Filling the DP Table  
The dynamic programming equation can be expressed as follows:
```text
dp[i][j] = 
    piles[i] - dp[i+1][j],         If the current player picks the pile at index i.
    piles[j] - dp[i][j - 1],       If the current player picks the pile at index j.
```
The negative sign in `"- dp[i+1][j]"` and `"- dp[i][j - 1]"`  is used to reverse the difference in the number of stones between the players when the turn changes.  
Since both players play optimally, the current player will choose the pile with the most stones.  
The equation will be:
```text
dp[i][j] = Math.max(piles[i] - dp[i+1][j], piles[j] - dp[i][j - 1])
```
Fill the DP table in a bottom-up matter.
The current problem (i, j) depends on the results of (i + 1, j) and (i, j - 1).   
By filling the table from the smallest subproblems to larger subproblems (from the end towards the start for i), 
we ensure that `dp[i + 1][j]` and `dp[i][j - 1]` are already computed when needed.  
Here is a random example:
```text
piles = 1 4 5 2 3 8 7 9 2 3
sum = 44
```

```text
indices:  0 1 2 3 4 5 6 7 8 9 
piles:    1 4 5 2 3 8 7 9 2 3

Step 1:
dp[0][0] = 1 
dp[1][1] = 4
...
dp[9][9] = 3


Step 2: 
dp[8][9] = Math.max( 2 - dp[9][9], 3 - dp[8][8] )

Step 3:
dp[7][8] = Math.max( 9 - dp[8][8], 2 - dp[7][7] ) 
dp[7][9] = Math.max( 9 - dp[8][9], 3 - dp[7][8] )

Step 4:
dp[6][7] = Math.max( 7 - dp[7][7], 9 - dp[6][6] ) 
dp[6][8] = Math.max( 7 - dp[7][8], 2 - dp[6][7] ) 
dp[6][9] = Math.max( 7 - dp[7][9], 3 - dp[6][8] ) 
...
```
#####  Result  
The result is determined based on whether the score difference for the entire array (`dp[0][length - 1]`) is positive, indicating that the first player Alice can secure a win.

Here is the solution:
```java
class Solution {
    public boolean stoneGame(int[] piles) {
        int length = piles.length;
        int[][] dp = new int[length][length];
        for (int i = 0; i < length; i++) {
            dp[i][i] = piles[i];
        }
        // Fill the DP table in a bottom-up matter. 
        for (int i = length - 2; i >= 0; i--) {
            for (int j = i + 1; j < length; j++) {
                dp[i][j] = Math.max(piles[i] - dp[i + 1][j], piles[j] - dp[i][j - 1]);
            }
        }
        return dp[0][length - 1] > 0;
    }
}
```
Time and Space Complexity
* Time Complexity: $ O(n^2)$ 

    The sum of iterations for both loop is:  
    $$ 1+2+3+...+(n-1) = \frac{(n-1) \times n}{2} =  O(n^2)$$
* Space Complexity: $ O(n^2)$ 

    The algorithm uses a two-dimensional array `dp` of size `n×n`, where `n` is the length of the input array `piles`.
    The space required for this array is $ O(n^2)$ .
#### Optimized Dynamic Programming Solution
The computation of each cell `dp[i][j]` only depends on values from the current row `i` and the next row `i + 1`,
Thus, We can reuse values in a single one-dimensional array.
```java
class Solution {
    public boolean stoneGame(int[] piles) {
        int length = piles.length;
        int[] dp = new int[length];
        System.arraycopy(piles, 0, dp, 0, length);
        for (int i = length - 2; i >= 0; i--) {
            for (int j = i + 1; j < length; j++) {
                dp[j] = Math.max(piles[i] - dp[j], piles[j] - dp[j - 1]);
            }
        }
        return dp[length - 1] > 0;
    }
}
```
Time and Space Complexity
* Time Complexity: $ O(n^2)$ 

    The sum of iterations for both loop is:  
    $$ 1+2+3+...+(n-1) = \frac{(n-1) \times n}{2} =  O(n^2)$$
* Space Complexity: $ O(n) $  

    The size of one-dimensional array `dp` is `n`, corresponding to the length of the input array.
    The space requried for this array is $ O(n) $.
## Target Sum
[Back to Top](#table-of-contents) 
### Overview
You are given an integer array `nums` and an integer `target`.  
You want to build an expression out of nums by adding one of the symbols `'+'` and `'-'` 
before each integer in nums and then concatenate all the integers.

+ For example, if `nums = [2, 1]`, you can add a `'+'` before `2` and a `'-'` before `1` and concatenate 
them to build the expression `"+2-1"`.
+ Return the number of different expressions that you can build, which evaluates to target.

Example 1:  
    Input: nums = [1,1,1,1,1], target = 3
    Output: 5
    Explanation: There are 5 ways to assign symbols to make the sum of nums be target 3.
    -1 + 1 + 1 + 1 + 1 = 3
    +1 - 1 + 1 + 1 + 1 = 3
    +1 + 1 - 1 + 1 + 1 = 3
    +1 + 1 + 1 - 1 + 1 = 3
    +1 + 1 + 1 + 1 - 1 = 3

**Example 2:**
    Input: nums = [1], target = 1
    Output: 1

**Constraints:**
* 1 <= nums.length <= 20
* 0 <= nums[i] <= 1000
* 0 <= sum(nums[i]) <= 1000
* -1000 <= target <= 1000
### Analysis
#### Depth-first Search Solution
Each element of the array nums can be added either a `+` or `-` sign, 
resulting in `2` choices per element and a total of 2<sup>n</sup> combinations for n elements.  
Use depth-first search to iterate over each combination and maintain a counter `targetSum` shared in each search path 
to count the total number of valid paths that achieve the target sum.

```java
public class Solution {
    private int targetSum=0;
    public int findTargetSumWays(int[] nums, int target) {
        this.dfs(nums, target, 0, 0);
        return this.targetSum;
    }

    /**
     * Find a valid path in which the sum of numbers in the array nums equals the target value.
     *
     * @param nums     Source array
     * @param target   Target sum
     * @param index    Current index
     */
    public void dfs(int[] nums, int target, int index, int sum){
        if(index>=nums.length) {
            if(sum==target)this.targetSum++;
            return;
        }
        dfs(nums, target, index+1, sum+nums[index]);
        dfs(nums, target, index+1, sum-nums[index]);
    }
}
```
Time and Space Complexity
* Time Complexity: $ O(2^n)$ 

    The total number of recursive calls is proportional to 2<sup>n</sup>, 
    as each element can either contribute positively or negatively to the sum.
* Space Complexity: $ O(n) $ (for the recursion stack)

Define the sum of the elements of the array `nums` as `sum`, the sum of the elements with a `-` sign is `neg`.  
According to the conditions, we can get the following expression:   
`(sum − neg) − neg = target`

Using the above expression, we can directly search for the sum of negative numbers, `neg`.
If the sum of numbers exceed `neg`, this case can be ignored,
unlike the initial solution, where each DFS path must be fully traversed.  

Here’s an example that uses the above expression:
```text
nums = 7 9 8 3 4 5 4 1 9 2 8 7  
sum = 67  
target =  67 + (- 8 - 4 -5 - 1 - 2 - 7)*2 = 13  
neg = (- 8 - 4 -5 - 1 - 2 - 7) = 27
```
The initial solution can be optimized as follows:
```java
public class Solution {
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
        // Use a map to store results for memoization
        Map<String, Integer> memo = new HashMap<>();

        // Start DFS with memoization
        return dfs(nums, neg, nums.length - 1, memo);
    }

    /**
     * Helper DFS method with memoization to count subsets summing to 'neg'.
     *
     * @param nums   The source array.
     * @param neg    The current target for the negative sum.
     * @param index  The current index in the array.
     * @param memo   The memoization map.
     * @return The number of ways to achieve the target neg sum.
     */
    private int dfs(int[] nums, int neg, int index, Map<String, Integer> memo) {
        // Base cases
        if (neg == 0 && index == -1) return 1; // Valid subset found
        if (index < 0) return 0; // No valid subset

        // Generate a unique key for memoization
        String key = index + "," + neg;

        // Check if result is already calculated
        if (memo.containsKey(key)) {
            return memo.get(key);
        }

        // Exclude current number or include it in the negative subset
        int exclude = dfs(nums, neg, index - 1, memo);
        int include = 0;
        if (neg >= nums[index]) {
            include = dfs(nums, neg - nums[index], index - 1, memo);
        }

        // Store the result in memo and return the sum of both choices
        int result = exclude + include;
        memo.put(key, result);
        return result;
    }
}
```
Time and Space Complexity
* Time Complexity: $ O(2^n)$  
    
    The worst-case time complexity remains the same, but it is faster than the original solution in general cases.
* Space Complexity: $ O(n) $

#### Dynamic Programming Solution
Define a two-dimensional array `dp`, where `dp[i][j]` represents the number of **solutions** 
to select elements from the first `i` numbers of the array nums so that the sum of these elements is equal to `j`.  

When `i=0`, there are no elements to select.   
If `j=0`, The sum of elements can only be `0`, so the corresponding number of solutions is `1`.  
If `j>=1`, the corresponding number of solutions is `0`.  
So the boundary conditions is:
```text
d[0][j]=
    1,   j = 0
    0,   j > 0
```
Define the length of the array `nums` to be `n`, so the final condition is `dp[n][neg]`.

The dynamic programming equation can be expressed as follows:
```text
dp[i][j]=
    dp[i−1][j],                          j < nums[i]
    dp[i−1][j] + dp[i−1][j−nums[i]],     j >= nums[i] 
```
If `j < nums[i]`, the current element must not be selected, ensuring that the sum of the selected numbers 
in the array `nums` does not exceed `j`.  
if `j>=nums[i]` and the current element is selected, the remaining sum to find in the 
first `i-1` elements is `j-nums[i]`.  
if `j>=nums[i]` and the current element is not selected, the result remains the same as `d[i-1][j]`.

Let's use the original example to demonstrate the execution process:
```text
nums = 7 9 8 3 4 5 4 1 9 2 8 7  
sum = 67  
target =  67 + (- 8 - 4 -5 - 1 - 2 - 7)*2 = 13  
neg = (- 8 - 4 -5 - 1 - 2 - 7) = 27
```
```text
i:        1 2 3 4 5 6 7 8 9 10 11 12
nums:     7 9 8 3 4 5 4 1 9 2  8  7 

Step 1:
dp[12][27] = dp[11][27] + dp[11][20]

Step 2:
dp[11][27] = dp[10][27] + dp[10][19]
dp[11][20] = dp[10][20] + dp[10][12]

Step 3:
dp[10][27] = dp[10][27] + dp[10][25]
dp[10][19] = dp[10][19] + dp[10][17]
dp[10][20] = dp[10][20] + dp[10][18]
dp[10][12] = dp[10][12] + dp[10][10]
...

Step n:
dp[0][0]=1
dp[0][27]=0  (j>0)
```
The execution process described above, which starts from the end of the array `nums`, 
resembles the Depth-First Search solution. 
However, if we traverse `nums` in the usual left-to-right order, 
we should calculate and store the potential negative values for each element in the `dp` array in advance,
allowing the subsequent `dp` items to access them.

Below is the solution that traverse the array `nums` in the usual order:
```text
Constraints:
    1 <= nums.length <= 20
    0 <= nums[i] <= 1000
    0 <= sum(nums[i]) <= 1000
    -1000 <= target <= 1000
```
```text
i:        1 2 3 4 5 6 7 8 9 10 11 12
nums:     7 9 8 3 4 5 4 1 9 2  8  7 
dp[0][0] = 1

Step 1:
dp[1][0->27] = dp[0][0->27]   (The current element is not selected)
dp[1][7->27] += dp[0][0->20]  (The current element has been selected)

Step 2:
dp[2][0->27] = dp=[1][0->27]
dp[2][9->27] += dp=[1][0->18]

Step 3:
dp[3][0->27] = dp=[2][0->27]
dp[3][8->27] += dp=[2][0->19]

...

Step 3:
dp[n][neg]

```
Use a two-dimensional array to store DP results. The solution is as follows:
```java
class Solution {
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
        int n = nums.length, neg = diff / 2;
        int[][] dp = new int[n + 1][neg + 1];
        dp[0][0] = 1;
        for (int i = 1; i <= n; i++) {
            int num = nums[i - 1];
            for (int j = 0; j <= neg; j++) {
                dp[i][j] = dp[i - 1][j];
                if (j >= num) {
                    dp[i][j] += dp[i - 1][j - num];
                }
            }
        }
        return dp[n][neg];
    }
}
```
Time and Space Complexity
* Time Complexity: $ O(n×neg) $   (with neg being dependent on the input values).  
* Space Complexity: $ O(n×neg) $

Since the current `dp` expression is only related to the previous one, 
the `dp` array can be simplified to a one-dimensional array:
```java
public class Solution {
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
        int[] dp = new int[neg + 1];
        dp[0] = 1;
        for (int num : nums) {
            for (int j = neg; j >= num; j--) {
                dp[j] += dp[j - num];
            }
        }
        return dp[neg];
    }
}
```
Time and Space Complexity
* Time Complexity: $ O(n×neg) $  (with neg being dependent on the input values).  
* Space Complexity: $ O(neg) $
# Sliding Window
## Find the Longest Equal Subarray
[Back to Top](#table-of-contents)  
### Overview
You are given a **0-indexed** integer array nums and an integer `k`.  
A subarray is called **equal** if all of its elements are equal. Note that the empty subarray is an **equal** subarray.  
Return the length of the **longest** possible equal subarray after deleting **at most** `k` elements from `nums`.  
A **subarray** is a contiguous, possibly empty sequence of elements within an array.

**Example 1:**
> Input: nums = [1,3,2,3,1,3], k = 3  
> Output: 3  
> Explanation:   
> It's optimal to delete the elements at index 2 and index 4.  
> After deleting them, nums becomes equal to [1, 3, 3, 3].  
> The longest equal subarray starts at i = 1 and ends at j = 3 with length equal to 3.  
> It can be proven that no longer equal subarrays can be created.

**Example 2:**
> Input: nums = [1,1,2,2,1,1], k = 2  
> Output: 4  
> Explanation:   
> It's optimal to delete the elements at index 2 and index 3.  
> After deleting them, nums becomes equal to [1, 1, 1, 1].  
> The array itself is an equal subarray, so the answer is 4.  
> It can be proven that no longer equal subarrays can be created.

**Constraints:**  
* 1 <= nums.length <= 105
* 1 <= nums[i] <= nums.length
* 0 <= k <= nums.length
### Analysis
Finding the longest possible equal subarray involves counting the number of identical numbers.   
Since we can delete at most `k` elements, 
we need to count the number of other numbers between the identical numbers to determine how many deletions are required.

//TODO Analyze the sliding window process.

```java
class Solution {
  public int longestEqualSubarray(List<Integer> nums, int k) {
    // Define an array to store the quantity of equal values. If the values in the array nums are large, use HashMap.
    int[] arr = new int[nums.size() + 1];
    int left = 0;
    int res = 0; // The final result
    int num = 0; // The count of unequal numbers in the range from left (inclusive) to index i (inclusive).
    // Traverse array nums
    for (int i = 0; i < nums.size(); i++) {
      // Count the number of times the current value occurs.
      ++arr[nums.get(i)];
      if (!Objects.equals(nums.get(i), nums.get(left))) {
        num++;
      }
      // When the count of unequal numbers exceeds the integer k, 
      // move the sliding window by one element and reduce the count of the current equal value by 1. 
      while (num > k) {
        arr[nums.get(left)]--;
        left++;
        num = i - left + 1 - arr[nums.get(left)];
      }
      res = Math.max(res, i - left + 1 - num);
    }
    while (left < nums.size() - 1) {
      arr[nums.get(left)]--;
      left++;
      num = nums.size() - left - arr[nums.get(left)];
      res = Math.max(res, nums.size() - left  - num);
    }
    return res;
  }
}
```
//TODO Analyze the standard solution and evaluate its time and space complexity.
# String
## License Key Formatting
[Back to Top](#table-of-contents)  
### Overview
You are given a license key represented as a string `s` that consists of only alphanumeric characters and dashes.  
The string is separated into `n + 1` groups by `n` dashes. 
You are also given an integer `k`.

We want to reformat the string `s` such that each group contains exactly `k` characters, 
except for the first group, which could be shorter than `k` but still must contain at least one character.   
Furthermore, there must be a dash inserted between two groups, and you should convert all lowercase letters to uppercase.

Return the reformatted license key.

**Example 1:**
> Input: s = "5F3Z-2e-9-w", k = 4  
> Output: "5F3Z-2E9W"  
> Explanation: The string s has been split into two parts, each part has 4 characters.   
> Note that the two extra dashes are not needed and can be removed.

**Example 2:**
> Input: s = "2-5g-3-J", k = 2  
> Output: "2-5G-3J"  
> Explanation: The string s has been split into three parts, each part has 2 characters except the first part as it could be shorter as mentioned above.
 

**Constraints:**
* 1 <= s.length <= 105
* s consists of English letters, digits, and dashes '-'.
* 1 <= k <= 104
### Analysis
First, capitalize the entire string and remove all `'-'` characters.
Determine the length of the first substring using `s.length % k`, then append the remaining substrings of length `k`.
```java
class Solution {
    public String licenseKeyFormatting(String s, int k) {
        // Capitalize the entire string directly.
        s=s.toUpperCase().replaceAll("-","");
        int firstLen=s.length()%k;
        char[] charArr=s.toCharArray();
        // Initialize the StringBuilder object with the first string whose length matches the remainder.
        StringBuilder sb=new StringBuilder(s.substring(0,firstLen));
        // Append other strings.
        for(int i=firstLen; i<s.length(); i+=k){
            if(i!=0)sb.append('-');
            sb.append(s.substring(i,i+k));
        }
        return sb.toString();
    }
}
```
Time and Space Complexity
* Time Complexity: $ O(n) $

    The time complexity of methods `toUpperCase, replaceAll, toCharArray` are all $ O(n) $.  
    Additionally, the time complexity for `substring` is O(m), where m is the length of the substring (endIndex - startIndex).  
    Since each iteration takes *O*(k) time where k is the length of the sliced substring and there are `(n-firstLen)/k` iterations, the loop takes $ O(n) $.
    Therefore, the total time complexity is $ O(n) $.
* Space Complexity: $ O(n) $
    
    `s.toCharArray()` creates a new character array of size $ O(n) $,
    `StringBuilder sb` stores the result string, which can also be of size $ O(n) $.  
    Therefore, the total space complexity is $ O(n) $
# Uncategorized Problems
## Maximize Value of Function in a Ball Passing Game
[Back to Top](#table-of-contents)  
### Overview
You are given an integer array `receiver` of length `n` and an integer `k`. 
`n` players are playing a ball-passing game.

You choose the starting player, `i`. The game proceeds as follows:   

player `i` passes the ball to player `receiver[i]`, who then passes it to `receiver[receiver[i]]`, 
and so on, for `k` passes in total. The game's score is the sum of the indices of the players who touched the ball, 
including repetitions, i.e. `i + receiver[i] + receiver[receiver[i]] + ... + receiver(k)[i]`.

Return the `maximum` possible score.

**Notes:**
* `receiver` may contain duplicates.

* `receiver[i]` may be equal to `i`.


**Example 1:**
> **Input:** `receiver = [2, 0, 1]`, `k = 4`  
> **Output:** `6`  
> **Explanation:**  
> Starting with player `i = 2`, the initial score is `2`.
> | Pass | Sender Index | Receiver Index | Score |
> |------|--------------|----------------|-------|
> | 1    | 2            | 1              | 3     |
> | 2    | 1            | 0              | 3     |
> | 3    | 0            | 2              | 5     |
> | 4    | 2            | 1              | 6     |

**Example 2:**
> **Input:** `receiver = [1, 1, 1, 2, 3]`, `k = 3`  
> **Output:** `10`  
> **Explanation:**  
> Starting with player `i = 4`, the initial score is `4`.  
> | Pass | Sender Index | Receiver Index | Score |
> |------|--------------|----------------|-------|
> | 1    | 4            | 3              | 7     |
> | 2    | 3            | 2              | 9     |
> | 3    | 2            | 1              | 10    |

**Constraints:**
* `1 <= receiver.length == n <= 105`

* `0 <= receiver[i] <= n - 1`

* `1 <= k <= 1010`
### Analysis
Using simple enumeration, we can calculate the sum at each index and compare them to determine the maximum sum.  
However, calculating for each index involves a significant amount of repeat computation, resulting in very low performance.

Enumeration Implementation:  
```java
class Solution {
    private long score=0;
    public long getMaxFunctionValue(List<Integer> receiver, long k) {
        long max=0;
        // Select an index to pass the ball
        for(int i=0; i<receiver.size(); i++){
            long sum=i;
            int ind=i;
            // Start passing the ball
            for(int j=0; j<k; j++){
                // Pass the ball to the next receiver
                ind=receiver.get(ind);
                sum+=ind;
            }
            max=Math.max(max,sum);
        }
        return max;
    }
}
```
Define a two-dimensional array `pa[i][x]` to store the receiver value `x` reached from the initial receiver after $ 2^i $ passings.
Initially, `pa[0][x]` is simply the direct receiver of `x`. 

Similarly, define a two-dimensional array `sum[i][x]` to store the cumulative sum of receiver values when making $ 2^i $ passings from receiver `x`.   
`sum[0][x]` is simply the receiver value at `receiver[x]`, as it represents a single passing.

We can pass the ball to a distant receiver by skipping $ 2^n $ passings, 
instead of passing to the next receiver, as we have stored the cumulative sums for each passing from from each receiver.

Using bitwise operations can significantly enhance the algorithm's performance:
* `k & k -1`
    
    The operation can result in:
    * The nearest lower even number to `k`.

        This operation clears the rightmost set bit, making `k` the nearest lower even number.
    * Zero when `k` is a power of two.

        The operation clears all bits when `k` is a power of two (like `8`, `16`, `32`, etc.)  
    
    Using `k & k-1` to skip passings, each iteration will have a default of `2` passings, 
    When only powers of two passings remain, directly retrieve the sum from the precomputed two-dimensional sum array.

    Even when `k` is not close to the power of `2`, the passing process remains optimized by skipping `2` passings in each iteration.

    Example 1:  
    * $ k = 6 $ (binary: `110`)  
    * $ k-1=5 $ (binary: `101`)  
    * `k & k-1` = `110 & 101 = 100 ` = `4`

    Example 2:  
    * $ k = 13 $ (binary: `1101`)
    * $ k-1=12 $ (binary: `1100`) 
    * `k & k-1` = `1101 & 1100 = 1100 ` = `12`

    Example 3:
    * $ k = 8 $ (binary: `1000`)
    * $ k-1=7 $ (binary: `0111`) 
    * `k & k-1` = `1000 & 0111 = 0000 ` = `0`


* `64 - Long.numberOfLeadingZeros(k)`

    Java `long` values are represented using `64` bits.
    By subtracting the number of leading zeros from `64`, 
    we determine the number of bits required to represent `k` in binary (its bit length).  

* `Long.numberOfTrailingZeros(k)`

//TODO Analyze passing process

#### Implementation
```java
class Solution {
    public long getMaxFunctionValue(List<Integer> receiver, long K) {
        int len = receiver.size();
        int m = 64 - Long.numberOfLeadingZeros(K); 
        var pa = new int[m][len];
        var sum = new long[m][len];
        // Populate the initial values
        for (int i = 0; i < len; i++) {
            pa[0][i] = receiver.get(i);
            sum[0][i] = receiver.get(i);
        }
        // Precompute the sum starting from each index incrementally, step by step.
        for (int i = 0; i < m - 1; i++) {
            // Traverse receivers
            for (int x = 0; x < len; x++) {
                int p = pa[i][x];
                pa[i + 1][x] = pa[i][p];
                sum[i + 1][x] = sum[i][x] + sum[i][p];
            }
        }
        long ans = 0;
        for (int i = 0; i < len; i++) {
            long s = i;
            int x = i;
            for (long k = K; k > 0; k &= k - 1) {
                int ctz = Long.numberOfTrailingZeros(k);
                s += sum[ctz][x];
                x = pa[ctz][x];
            }
            ans = Math.max(ans, s);
        }
        return ans;
    }
}
```
##### Time and Space Complexity
* Time Complexity: $ O(n \log n) $


* Space Complexity: $ O(n \log n) $

//TODO Compute Time and Space Complexity


## Find Number of Ways to Reach the K-th Stair
[Back to Top](#table-of-contents)  
### Overview
You are given a **non-negative** integer `k`. There exists a staircase with an infinite number of stairs, with the **lowest** stair numbered 0.

Alice has an integer `jump`, with an initial value of 0. 
She starts on stair 1 and wants to reach stair `k` using any number of operations. 
If she is on stair `i`, in one operation she can:

Go down to stair `i - 1`. This operation cannot be used consecutively or on stair 0.
Go up to stair $i + 2^{\text{jump}} $. And then, `jump` becomes `jump + 1`.
Return the total number of ways Alice can reach stair `k`.

Note that it is possible that Alice reaches the stair `k`, and performs some operations to reach the stair `k` again.

**Example 1:**
> Input: k = 0  
> Output: 2  
> Explanation:  
> The 2 possible ways of reaching stair 0 are:
> * Alice starts at stair 1.
>   * Using an operation of the first type, she goes down 1 stair to reach stair 0.
> * Alice starts at stair 1.
>   * Using an operation of the first type, she goes down 1 stair to reach stair 0.
>   * Using an operation of the second type, she goes up 20 stairs to reach stair 1.
>   * Using an operation of the first type, she goes down 1 stair to reach stair 0.

**Example 2:**
> Input: k = 1  
> Output: 4  
> Explanation:  
> The 4 possible ways of reaching stair 1 are:
> * Alice starts at stair 1. Alice is at stair 1.  
> * Alice starts at stair 1.  
>   * Using an operation of the first type, she goes down 1 stair to reach stair 0.  
>   * Using an operation of the second type, she goes up 20 stairs to reach stair 1.  
> * Alice starts at stair 1.
>   * Using an operation of the second type, she goes up 20 stairs to reach stair 2.
>   * Using an operation of the first type, she goes down 1 stair to reach stair 1.
> * Alice starts at stair 1.
>   * Using an operation of the first type, she goes down 1 stair to reach stair 0.
>   * Using an operation of the second type, she goes up 20 stairs to reach stair 1.
>   * Using an operation of the first type, she goes down 1 stair to reach stair 0.
>   * Using an operation of the second type, she goes up 21 stairs to reach stair 2.
>   * Using an operation of the first type, she goes down 1 stair to reach stair 1.

**Constraints:**
* 0 <= k <= 109

### Analysis
If Alice go up at each jump, the steps will be predictable:
$$ 2^0 + 2^1 + 2^2 + ... + 2^n = 2^{n+1}-1$$
The total jumps is `k`, so we need to calculate the nearest lower power of two to `k`, 


```text
k = 2

1 -> 2        
     up
1 -> 2 -> 1 -> 3 -> 2       
     up -> down -> up -> down
1 -> 0 -> 1 -> 3 -> 2  
     down - up - up - down
1 -> 0 -> 1 -> 0 -> 2
     down - up - down - up

k = 8
1  ->  8
```

#### Implementation
```java
class Solution {
    // 0 <= k <= 10^9
    public int waysToReachStair(int k) {
        int result=0;
        int equalJump=Integer.highestOnBit(k);
        if(equalJump==k)result++;
        // Get the nearest lower power of 2 to k.
        int overJump=Integer.highestOnBit(k+1)<<1;
        int upwardJumps=31 - Integer.numberOfLeadingZeros(k+1) + 1;
        int downwardJumps=overJump-k;
        // Insert these downward jumps between or after all upward jumps
        // 1 2 3 4 5 
        //  * *   *
    }
}
```

//TODO finish the solution.

## Climbing Stairs
[Back to Top](#table-of-contents)  
### Overview
You are climbing a staircase. It takes `n` steps to reach the top.

Each time you can either climb `1` or `2` steps. In how many distinct ways can you climb to the top?

Example 1:
> Input: n = 2  
> Output: 2  
> Explanation: There are two ways to climb to the top.
> 1. 1 step + 1 step  
> 2. 2 steps

Example 2:
> Input: n = 3  
> Output: 3  
> Explanation: There are three ways to climb to the top.  
> 1. 1 step + 1 step + 1 step  
> 2. 1 step + 2 steps  
> 3. 2 steps + 1 step


**Constraints:**
* 1 <= n <= 45

### Analysis
Here is a simple example when `n=5`:
```text
stair:            0 1 2 3 4 5 
ways:               1 2 3 5 8
```
Observe the above example, the number of ways to reach stair `n` is the sum of the number of ways to reach stairs `n-1` and `n-2`.
Thus, it follows the Fibonacci sequence.

#### Dynamic Programming Solution
Fibonacci sequence formula:
$$ F(n)=F(n-1)+F(n-2) $$
##### Initialization 
The number of ways to reach stair `1` is `1` and stair `2` is `2`, so:  
$$ F(1) = 1,  F(2) = 2 $$
##### Filling the DP Table
Since this process only depends on the previous two stairs, we can just define two variables to store the number of ways for the previous two stairs.
#### Implementation
```java
class Solution {
    public int climbStairs(int n) {
        if(n==1)return 1;
        if(n==2)return 2;
        int pre1=1;
        int pre2=2;
        int current=0;
        for(int i=3;i<=n;i++){
            current=pre1+pre2;
            pre1=pre2;
            pre2=current;
        }
        return current;

    }
}
```
##### Time and Space Complexity
* Time Complexity: $ O(n) $
* Space Complexity: $ O(1) $

