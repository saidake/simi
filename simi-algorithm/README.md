# Table of Contents
[Back to Main Project README](../README.md)
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

Here’s an example that uses the above expression:
```text
nums = 7 9 8 3 4 5 4 1 9 2 8 7  
sum = 67  
target =  67 + (- 8 - 4 -5 - 1 - 2 - 7)*2 = 13  
neg = (67-13)/2 = 27
```
Define a two-dimensional array dp, where `dp[i][j]` represents the number of **solutions** 
to select elements from the first `i` numbers of the array nums so that the sum of these elements is equal to `j`.  

When `i=0`, there are no elements to select.   
If `j=0`, The sum of elements can only be `0`, so the corresponding number of solutions is `1`.  
If `j>=1`, the corresponding number of solutions is `0`.  
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
If `j < nums[i]`, the current element cannot be selected, ensuring that the sum of the selected numbers 
in the array `nums` does not exceed `j`.  
if `j>=nums[i]` and the current element is selected, the remaining sum to find in the 
first `i-1` elements is `j-nums[i]`.  
if `j>=nums[i]` and the current element is not selected, the result remains the same as `d[i-1][j]`.

When the index is `i` and the current value is selected, We need to check the first `i-1` elements
to find the sum equal to `j-nums[i]`.  

Let's use the initial example to demonstrate the execution process:
```text
nums = 7 9 8 3 4 5 4 1 9 2 8 7  
sum = 67  
target =  67 + (- 8 - 4 -5 - 1 - 2 - 7)*2 = 13  
neg = (67-13)/2 = 27
```
```text
Step 1:
indexes:  0 1 2 3 4 5 6 7 8 9 10 11
nums:     7 9 8 3 4 5 4 1 9 2 8  7 
dp[11][27] = dp[10][27] + dp[10][20]

Step 2:
indexes:  0 1 2 3 4 5 6 7 8 9 10
nums:     7 9 8 3 4 5 4 1 9 2 8
dp[10][27] = dp[9][27] + dp[9][19]
dp[10][20] = dp[9][20] + dp[9][12]

Step 3:
indexes:  0 1 2 3 4 5 6 7 8 9 
nums:     7 9 8 3 4 5 4 1 9 2 
dp[9][27] = dp[8][27] + dp[8][25]
dp[9][19] = dp[8][19] + dp[8][17]
dp[9][20] = dp[8][20] + dp[8][18]
dp[9][12] = dp[8][12] + dp[8][10]
...
```
Since the current dp expression is only related to the previous one, 
the dp array can be simplified to a one-dimensional array: 
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
### Source Code
Check out [`TargetSum.java`](src/main/java/com/simi/algorithm/dynamicprogramming/TargetSum.java) for the source code.
