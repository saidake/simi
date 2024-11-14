# Table of Contents
[Back to Main Project README](README.md)
- [Dynamic Programming](#dynamic-programming)
    - [Stone Game](#stone-game)
    - [Target Sum](#target-sum)
- [Sliding Window](#sliding-window)
    - [Find the Longest Equal Subarray](#find-the-longest-equal-subarray)
- [String](#string)
    - [License Key Formatting](#license-key-formatting)
- [Uncategorized Problems](#uncategorized-problems)
    - [Decode Ways II](#decode-ways-ii)
# Dynamic Programming
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

```text
Example 1:
    Input: piles = [5,3,4,5]
    Output: true
    Explanation: 
        Alice starts first, and can only take the first 5 or the last 5.
        Say she takes the first 5, so that the row becomes [3, 4, 5].
        If Bob takes 3, then the board is [4, 5], and Alice takes 5 to win with 10 points.
        If Bob takes the last 5, then the board is [3, 4], and Alice takes 4 to win with 9 points.
        This demonstrated that taking the first 5 was a winning move for Alice, so we return true.
Example 2:
    Input: piles = [3,7,2,3]
    Output: true
Constraints:
    2 <= piles.length <= 500
    piles.length is even.
    1 <= piles[i] <= 500
    sum(piles[i]) is odd.
```
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
* Space Complexity: *O*(n) (for the recursion stack)
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
* Time Complexity: *O*(n<sup>2</sup>)  

    The sum of iterations for both loop is:  
    1+2+3+...+(n-1) = (n-1)×n/2 = *O*(n<sup>2</sup>)  
* Space Complexity: *O*(n<sup>2</sup>)  

    The algorithm uses a two-dimensional array `dp` of size n×n, where n is the length of the input array `piles`.
    The space required for this array is *O*(n<sup>2</sup>).
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
* Time Complexity: *O*(n<sup>2</sup>)  

    The sum of iterations for both loop is:  
    1+2+3+...+(n-1) = (n-1)×n/2 = *O*(n<sup>2</sup>)  
* Space Complexity: *O*(n)  

    The size of one-dimensional array `dp` is `n`, corresponding to the length of the input array.
    The space requried for this array is *O*(n).
## Target Sum
[Back to Top](#table-of-contents) 
### Overview
You are given an integer array `nums` and an integer `target`.  
You want to build an expression out of nums by adding one of the symbols `'+'` and `'-'` 
before each integer in nums and then concatenate all the integers.

+ For example, if `nums = [2, 1]`, you can add a `'+'` before `2` and a `'-'` before `1` and concatenate 
them to build the expression `"+2-1"`.
+ Return the number of different expressions that you can build, which evaluates to target.

```text
Example 1:  
    Input: nums = [1,1,1,1,1], target = 3
    Output: 5
    Explanation: There are 5 ways to assign symbols to make the sum of nums be target 3.
    -1 + 1 + 1 + 1 + 1 = 3
    +1 - 1 + 1 + 1 + 1 = 3
    +1 + 1 - 1 + 1 + 1 = 3
    +1 + 1 + 1 - 1 + 1 = 3
    +1 + 1 + 1 + 1 - 1 = 3
Example 2:
    Input: nums = [1], target = 1
    Output: 1

Constraints:
    1 <= nums.length <= 20
    0 <= nums[i] <= 1000
    0 <= sum(nums[i]) <= 1000
    -1000 <= target <= 1000
```
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
* Time Complexity: *O*(2<sup>n</sup>)  
    The total number of recursive calls is proportional to 2<sup>n</sup>, 
    as each element can either contribute positively or negatively to the sum.
* Space Complexity: *O*(n) (for the recursion stack)

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
* Time Complexity: *O*(2<sup>n</sup>)  (The worst-case time complexity remains the same, but it is faster than the original solution in general cases.)   
* Space Complexity: *O*(n)

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
* Time Complexity: *O*(n×neg)   (with neg being dependent on the input values).  
* Space Complexity: *O*(n×neg)

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
* Time Complexity: *O*(n×neg)   (with neg being dependent on the input values).  
* Space Complexity: *O*(neg)
# Sliding Window
## Find the Longest Equal Subarray
[Back to Top](#table-of-contents)  
### Overview
You are given a **0-indexed** integer array nums and an integer `k`.  
A subarray is called **equal** if all of its elements are equal. Note that the empty subarray is an **equal** subarray.  
Return the length of the **longest** possible equal subarray after deleting **at most** `k` elements from `nums`.  
A **subarray** is a contiguous, possibly empty sequence of elements within an array.
```text
Example 1:
    Input: nums = [1,3,2,3,1,3], k = 3
    Output: 3
    Explanation: 
        It's optimal to delete the elements at index 2 and index 4.
        After deleting them, nums becomes equal to [1, 3, 3, 3].
        The longest equal subarray starts at i = 1 and ends at j = 3 with length equal to 3.
        It can be proven that no longer equal subarrays can be created.
Example 2:
    Input: nums = [1,1,2,2,1,1], k = 2
    Output: 4
    Explanation: 
        It's optimal to delete the elements at index 2 and index 3.
        After deleting them, nums becomes equal to [1, 1, 1, 1].
        The array itself is an equal subarray, so the answer is 4.
        It can be proven that no longer equal subarrays can be created.
Constraints:
    1 <= nums.length <= 105
    1 <= nums[i] <= nums.length
    0 <= k <= nums.length
```
### Analysis
Finding the longest possible equal subarray involves counting the number of identical numbers.   
Since we can delete at most `k` elements, 
we need to count the number of other numbers between the identical numbers to determine how many deletions are required.
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
```text
Example 1:
    Input: s = "5F3Z-2e-9-w", k = 4
    Output: "5F3Z-2E9W"
    Explanation: The string s has been split into two parts, each part has 4 characters. 
    Note that the two extra dashes are not needed and can be removed.
Example 2:
    Input: s = "2-5g-3-J", k = 2
    Output: "2-5G-3J"
    Explanation: The string s has been split into three parts, each part has 2 characters except the first part as it could be shorter as mentioned above.
 

Constraints:
    1 <= s.length <= 105
    s consists of English letters, digits, and dashes '-'.
    1 <= k <= 104
```
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
* Time Complexity: *O*(n)  

    The time complexity of methods `toUpperCase, replaceAll, toCharArray` are all *O*(n).  
    Additionally, the time complexity for `substring` is O(m), where m is the length of the substring (endIndex - startIndex).  
    Since each iteration takes *O*(k) time where k is the length of the sliced substring and there are `(n-firstLen)/k` iterations, the loop takes *O*(n).
    Therefore, the total time complexity is *O*(n).
* Space Complexity: *O*(n)
    
    `s.toCharArray()` creates a new character array of size *O*(n),
    `StringBuilder sb` stores the result string, which can also be of size *O*(n).  
    Therefore, the total space complexity is *O*(n)
# Uncategorized Problems
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

Since the answer may be very large, return it **modulo** 10<sup>9</sup> + 7.
```text
Example 1:
    Input: s = "*"
    Output: 9
    Explanation: The encoded message can represent any of the encoded messages "1", "2", "3", "4", "5", "6", "7", "8", or "9".
    Each of these can be decoded to the strings "A", "B", "C", "D", "E", "F", "G", "H", and "I" respectively.
    Hence, there are a total of 9 ways to decode "*".
Example 2:
    Input: s = "1*"
    Output: 18
    Explanation: The encoded message can represent any of the encoded messages "11", "12", "13", "14", "15", "16", "17", "18", or "19".
    Each of these encoded messages have 2 ways to be decoded (e.g. "11" can be decoded to "AA" or "K").
    Hence, there are a total of 9 * 2 = 18 ways to decode "1*".
Example 3:
    Input: s = "2*"
    Output: 15
    Explanation: The encoded message can represent any of the encoded messages "21", "22", "23", "24", "25", "26", "27", "28", or "29".
    "21", "22", "23", "24", "25", and "26" have 2 ways of being decoded, but "27", "28", and "29" only have 1 way.
    Hence, there are a total of (6 * 2) + (3 * 1) = 12 + 3 = 15 ways to decode "2*".

Constraints:
    1 <= s.length <= 105
    s[i] is a digit or '*'.
```
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
##### Initialization 
Define a one-dimensional array `dp`, where `dp[i]` represents the number of ways to decode the  the string `s` from index `0` to index `i`.  
When i=0, there is only one number can be choosed, so `dp[0] = 1`.
#####  Filling the DP Table  
The dynamic programming equation can be expressed as follows:
```text
dp[i] = 
    dp[i-1],            s[i] = (7 8, 9) and s[i-1]!=1
    dp[i-2]×2,          s[i] = (7 8, 9) and s[i-1]=1
    dp[i-1],                s[i] = 0-6 and s[i-1] = 3-9 or 0
    dp[i-2]×2,              s[i] = 0-6 and s[i-1] = 1 or 2
    dp[i-2]×11,             s[i] = 0-6 and s[i-1] = *
    dp[i-1]×9,                  s[i] = * and s[i-1] = 3-9 or 0
    dp[i-2]×18,                 s[i] = * and s[i-1] = 1 
    dp[i-2]×15,                 s[i] = * and s[i-1] = 2
    dp[i-1]*9+dp[1-2]*15,       s[i] = * and s[i-1] = *
```
#####  Result
The dp[length-1] is the number of ways to decode the string `s`.

Here is the solution:
```java
class Solution {
    public int numDecodings(String s) {
        int len=s.length();
        int[] dp=new int[len+1];
        dp[0]=1;
        char[] chars=s.toCharArray();
        //Initialize dp array
        if(chars[0]!='*')dp[1]=1;
        else dp[1]=9;
        //traverse array chars
        for(int i=2; i<=len; i++){
            int pre=Character.getNumericValue(chars[i-2]);
            int val=Character.getNumericValue(chars[i-1]);
            //Calculate dp value
            if(val>=7){
                if(pre!=1)dp[i]=dp[i-1];
                else dp[i]=dp[i-2]*2;
            }else if(val>=0){
                if(pre>=3||pre==0){
                    dp[i]=dp[i-1];
                }else if(pre!=-1){
                    dp[i]=dp[i-2]*2;
                }else{
                    dp[i]=dp[i-1]+dp[i-2]*2;
                }
            }else{
                if(pre>=3||pre==0){
                    dp[i]=dp[i-1]*9;
                }else if(pre==1){
                    dp[i]=dp[i-2]*18;
                }else if(pre==2){
                    dp[i]=dp[i-2]*15;
                }else{
                    // 1* = 9+9
                    // 2* = 9+6
                    // "**"  ->  9×9 +9 +6  ->   96
                    // "***" ->  96×9 +    ->  864
                    dp[i]=dp[i-1]*9+dp[i-2]*15;
                }
            }
        }
        return (int)(dp[len]%(Math.pow(10,9)+7));
    }
}
```

