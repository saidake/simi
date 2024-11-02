# Table of Contents
[Back to Main Project README](README.md)
- [Dynamic Programming](#dynamic-programming)
    - [Target Sum](#target-sum)
# Dynamic Programming
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
Define the sum of the elements of the array `nums` as `sum`, the sum of the elements with a `-` sign is `neg`.  
According to the conditions, we can get the following expression:   
`(sum − neg) − neg = target`  

Random Example:
```text
nums = 7 9 8 3 4 5 4 1 9 2 8 7  
sum = 67  
target =  67 + (- 8 - 4 -5 - 1 - 2 - 7)*2 = 13  
neg = (67-13)/2 = 27
```
Define a two-dimensional array dp, where `dp[i][j]` represents the number of **solutions** 
to select elements from the first `i` numbers of the array nums so that the sum of these elements is equal to `j`.  
When there is no elements to be selected, the sum of elements can only be `0`, 
and the corresponding number of solutions is `1`.  
If `j` is greater than or equal to `1`, the corresponding number of solutions is `0`.  
So the boundary conditions is:
```text
d[0][j]=
    1,   j=0
    0,   j>=1
```
Define the length of the array nums to be `n`, so the final condition is `dp[n][neg]`.

The dynamic programming equation can be expressed as follows:
```text
dp[i][j]=
    dp[i−1][j],                          j < nums[i]
    dp[i−1][j] + dp[i−1][j−nums[i]],     j >= nums[i] 
```

```text
 num = 7, j = 27 -> 7
     dp[27] = dp[27] + dp[20] = 0
     ...
     dp[7] = dp[7] + dp[0] = 1 (dp[7]=1)
```
```text
 num = 9, j = 27 -> 9
     dp[27] = dp[27] + dp[16] = 0
     ...
     dp[16] = dp[16] + dp[7] = 1 (dp[16]=1)
     dp[9] = dp[9] + dp[0] = 1 (dp[9]=1)
```
```text
 num = 8, j = 27 -> 8
     dp[27] = dp[27] + dp[19] = 0
     ...
     dp[24] = dp[24] + dp[16] = 1 (dp[24]=1)
     dp[17] = dp[17] + dp[9] = 1 (dp[17]=1)
     dp[15] = dp[15] + dp[7] = 1 (dp[15]=1)
     dp[8] = dp[8] + dp[0] = 1 (dp[8]=1)
```
### Implementation
Check out [`TargetSum.java`](simi-algorithm/src/main/java/com/simi/algorithm/dynamicprogramming/TargetSum.java) for the implementation.
