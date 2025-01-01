# Table of Contents
[Back to Main Project README](../README.md)
- [Array](#array)
    - [Array Partition](#array-partition)
- [Difference Array](#difference-array)
    - [Jump Game VII](#jump-game-vii)
- [Dynamic Programming](#dynamic-programming)
    - [Climbing Stairs](#climbing-stairs)
    - [Decode Ways II](#decode-ways-ii)
    - [Maximize Value of Function in a Ball Passing Game](#maximize-value-of-function-in-a-ball-passing-game)
    - [Stone Game](#stone-game)
    - [Target Sum](#target-sum)
- [Math](#math)
    - [Construct the Minimum Bitwise Array II](#construct-the-minimum-bitwise-array-ii)
    - [Find Number of Ways to Reach the K-th Stair](#find-number-of-ways-to-reach-the-k-th-stair)
    - [Minimum Moves to Capture The Queen](#minimum-moves-to-capture-the-queen)
- [Precomputation](#precomputation)
    - [Range Product Queries of Powers](#range-product-queries-of-powers)
- [Sliding Window](#sliding-window)
    - [Find the Longest Equal Subarray](#find-the-longest-equal-subarray)
- [String](#string)
    - [License Key Formatting](#license-key-formatting)
- [Traversal](#traversal)
    - [Maximum Number of Operations With the Same Score I](#maximum-number-of-operations-with-the-same-score-i)
- [SQL](#sql)
    - [Odd and Even Transactions](#odd-and-even-transactions)
- [Uncategorized Problems](#uncategorized-problems)
    - [Distribute Elements Into Two Arrays II](#distribute-elements-into-two-arrays-ii)
    - [Merge Sorted Array](#merge-sorted-array)
    - [Max Difference You Can Get From Changing an Integer](#max-difference-you-can-get-from-changing-an-integer)
    - [Egg Drop With 2 Eggs and N Floors](#egg-drop-with-2-eggs-and-n-floors)
    - [Maximum Length of Subarray With Positive Product](#maximum-length-of-subarray-with-positive-product)
    - [Add Edges to Make Degrees of All Nodes Even](#add-edges-to-make-degrees-of-all-nodes-even)
# Array
## Array Partition
[Back to Top](#table-of-contents)  
### Overview
Given an integer array `nums` of 2n integers, 
group these integers into `n` pairs `(a1, b1), (a2, b2), ..., (an, bn)` such that the sum of `min(ai, bi)` for all `i` is maximized. 
Return the maximized sum.

**Example 1:**
> **Input:** nums = [1,4,3,2]  
> **Output:** 4  
> **Explanation:** All possible pairings (ignoring the ordering of elements) are:
> 1. (1, 4), (2, 3) -> min(1, 4) + min(2, 3) = 1 + 2 = 3
> 2. (1, 3), (2, 4) -> min(1, 3) + min(2, 4) = 1 + 2 = 3
> 3. (1, 2), (3, 4) -> min(1, 2) + min(3, 4) = 1 + 3 = 4
> 
> So the maximum possible sum is 4.

**Example 2:**
> **Input:** nums = [6,2,6,5,1,2]  
> **Output:** 9  
> **Explanation:**   
> The optimal pairing is (2, 1), (2, 5), (6, 6). min(2, 1) + min(2, 5) + min(6, 6) = 1 + 2 + 6 = 9.

**Constraints:**
* `1 <= n <= 10^4`
* `nums.length == 2 * n`
* `-10^4 <= nums[i] <= 10^4`
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
#### Time and Space Complexity
* Time Complexity: $ O(n \log n) $

    The time complexity of `Arrays.sort(nums)` is $ O(n \log n) $.  
    The loop iterates through the array with a step of 2, so it runs $ 𝑛/2 $ times. 
    The time complexity of this loop is $ O(n) $.  
    The sorting step dominates the iteration step. 
    Hence, The total time complexity is $ O(n \log n) $.
* Space Complexity: $ O(1) $

    The `Arrays.sort()` method uses $ O(1) $ space for primitive data types like integers in Java, as it utilizes a variation of quicksort (dual-pivot quicksort).
    There is no additional space used apart from the input array and a few variables.   
    Therefore, the total space complexity is $ O(1) $.
# Difference Array
## Jump Game VII
[Back to Top](#table-of-contents)  
### Overview
You are given a 0-indexed binary string `s` and two integers `minJump` and `maxJump`. 
In the beginning, you are standing at index `0`, which is equal to `'0'`. 
You can move from index `i` to index `j` if the following conditions are fulfilled:
* `i + minJump <= j <= min(i + maxJump, s.length - 1)`, and
* `s[j] == '0'`. 

Return true if you can reach index `s.length - 1` in `s`, or `false` otherwise.

**Example 1:**

> **Input:** s = "011010", minJump = 2, maxJump = 3  
> **Output:** true  
> **Explanation:**  
> In the first step, move from index 0 to index 3.   
> In the second step, move from index 3 to index 5.

**Example 2:** 

> **Input:** s = "01101110", minJump = 2, maxJump = 3  
> **Output:** false

**Constraints:** 
* `2 <= s.length <= 10^5`
* `s[i]` is either `'0'` or `'1'`.
* `s[0] == '0'`
* `1 <= minJump <= maxJump < s.length`

### Analysis
The `minJump` and `maxJump` define the reachable range for each index `i` in the string `s`. 
Directly applying dynamic programming or depth-first search to traverse the entire range would result in high time complexity.

### Difference Array Solution
Instead of checking the entire reachable range, use a `diff` array to mark the start(`+1`) and the end(`-1`) of each reachable range.  
Utilize an accumulatable falg `acc` to track the reachable range:
* Increment `acc` by `1` (`acc += 1`) upon entering a range 
* Decrement `acc` by `1` (`acc -= 1`) upon leaving.

Example:
```text
Two non-overlapping reachable areas:
minJump = 3, maxJump = 4
s:      0 1 1 0 1 1 1 1 1 1
              a a   b b
acc:          + -   + -

Two overlapping reachable areas:
minJump = 2, maxJump = 5
s:      0 1 0 1 1 1 1 1 1 1
            a a a a  
                b b b b
acc:        +     -
                +     -
```
In both cases, the `diff` array enables distinguishing whether current index is within a reachable range, even when multiple ranges overlap.
#### Implementation
```java
class Solution {

    public boolean canReach(String s, int minJump, int maxJump) {
        int len = s.length();
        if ( s.charAt(len - 1) == '1' || minJump > len) {
            return false;
        }
        int[] diff = new int[len];
        diff[0] = 1;
        diff[1] = -1;
        int acc = 0;
        // Accumulated prefix sum to track reachability
        for (int i = 0; i < len; i++) {
            acc += diff[i];
            if (acc > 0 && s.charAt(i) == '0') {
                // Mark the start of the reachable range from i + minJump
                if (i + minJump < len) {
                    diff[i + minJump] += 1;
                } 
                // Mark the end of the reachable range after i + maxJump
                if (i + maxJump + 1 < len) {
                    diff[i + maxJump + 1] -= 1;
                }
            }
        }
        return acc > 0;
    }
}
```
#### Time and Space Complexity
* Time Complexity: $ O(n) $ 

    The `for` loop iterates over each character in the string `s`, resulting in a time complexity of $O(n)$.
* Space Complexity: $ O(n) $

    The `diff` array, which has a size of `len` (the length of the string `s`), 
    contributes $O(n)$ to the space complexity. 
    
    Other variables require constant space, resulting in an overall space complexity of $O(n)$.

### Sliding Window Solution
Maintain a sliding window from `left` to `right` to track the count of indices `nbp` for a specific index `i` as the sliding window moves along the string `s`.  
if  `nbp > 0`, the index `i` is reachable from the sliding window range, otherwise, it is not.  
When the window's length exceeds `maxJump - minJump + 1`, remove the character at the left boundary and update `npb` accordingly.

Example: 
```text
minJump = 3, maxJump = 5

Step 1 (Initialization):
    index: 0 1 2 3 4 5 6 7 8 9 
    s:     0 0 1 1 0 1 0 0 0 0 
           L
           R     i
    nbp = 1
    dp[0] = true
    dp[1] = false (initialized to false by default)
    dp[2] = false (initialized to false by default)
    dp[3] = false

Step 2:
    index: 0 1 2 3 4 5 6 7 8 9 
    s:     0 0 1 1 0 1 0 0 0 0 
           L
             R     i
    nbp = 1
    dp[4] = true

Step 3:
    index: 0 1 2 3 4 5 6 7 8 9 
    s:     0 0 1 1 0 1 0 0 0 0 
           L
               R     i
    nbp = 1
    dp[5] = false

Step 4:
    index: 0 1 2 3 4 5 6 7 8 9 
    s:     0 0 1 1 0 1 0 0 0 0 
             L
                 R     i
    nbp = 0 (decremented by 1 as dp[0] is true and has been excluded)
    dp[6] = false

Step 5:
    index: 0 1 2 3 4 5 6 7 8 9 
    s:     0 0 1 1 0 1 0 0 0 0 
               L
                   R     i
    nbp = 1 (incremented by 1 as dp[4] is true and has been included)
    dp[7] = true

... 

Step n:
    index: 0 1 2 3 4 5 6 7 8 9 
    s:     0 0 1 1 0 1 0 0 0 0 
                   L
                       R     i
    Return dp[9]
```
#### Implementation
```java
class Solution {
    public boolean canReach(String s, int minJump, int maxJump) {
        int len=s.length();
        if (s.charAt(len - 1) == '1') return false;
        boolean[] dp = new boolean[len];
        dp[0] = true;

        int nbp = 1;
        int left = 0;
        int right = 0;
        // Check if indices are reachable from index minJump
        for (int i = minJump; i < len; i++) {
            if (s.charAt(i) == '0') {
                if (nbp > 0) {
                    dp[i] = true;
                }
            }
            // Move the left boundary of the sliding window rightward by 1 when its length reaches `maxJump - minJump + 1`.
            if (right - left + 1 == maxJump - minJump + 1) {
                if (dp[left]) {
                    nbp--;
                }
                left++;
            }
            // Move the right boundary of the sliding window rightward duiring the traversal
            right++;
            if (dp[right]) {
                nbp++;
            }
        }
        return dp[len - 1];
    }
}
```
#### Time and Space Complexity
* Time Complexity: $ O(n) $ 
    
    The `for` loop iterates from `minJump` to `len` (the length of the string s), resulting in a time complexity of $O(n)$.

* Space Complexity: $ O(n) $

    The `dp` array has a size of len, contributing $O(n)$ to the space complexity.

# Dynamic Programming
## Climbing Stairs
[Back to Top](#table-of-contents)  
### Overview
You are climbing a staircase. It takes `n` steps to reach the top.

Each time you can either climb `1` or `2` steps. In how many distinct ways can you climb to the top?

**Example 1:**
> **Input:** n = 2  
> **Output:** 2  
> **Explanation:** There are two ways to climb to the top.
> 1. 1 step + 1 step  
> 2. 2 steps

**Example 2:**
> **Input:** n = 3  
> **Output:** 3  
> **Explanation:** There are three ways to climb to the top.  
> 1. 1 step + 1 step + 1 step  
> 2. 1 step + 2 steps  
> 3. 2 steps + 1 step


**Constraints:**
* `1 <= n <= 45`

### Analysis
Here is a simple example when `n=5`:
```text
stair:            0 1 2 3 4 5 
ways:               1 2 3 5 8
```
Observe the above example, the number of ways to reach stair `n` is the sum of the number of ways to reach stairs `n-1` and `n-2`.
Thus, it follows the Fibonacci sequence.

### Dynamic Programming Solution
Fibonacci sequence formula:
$$ F(n)=F(n-1)+F(n-2) $$
#### Initialization 
The number of ways to reach stair `1` is `1` and stair `2` is `2`, so:  
$$ F(1) = 1,  F(2) = 2 $$
#### Filling the DP Table
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
        // Calculate the number of ways to reach a specific stair.
        for(int i=3;i<=n;i++){
            current=pre1+pre2;
            pre1=pre2;
            pre2=current;
        }
        return current;

    }
}
```
#### Time and Space Complexity
* Time Complexity: $ O(n) $

    The `for` loop iterate over the stairs starting from `3`, yielding a time complexity of $O(n)$.
* Space Complexity: $ O(1) $
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
> **Input:** s = "*"  
> **Output:** 9  
> **Explanation:** The encoded message can represent any of the encoded messages "1", "2", "3","4", "5", "6", "7", "8", or "9".  
> Each of these can be decoded to the strings "A", "B", "C", "D", "E", "F", "G", "H", and"I" respectively.  
> Hence, there are a total of 9 ways to decode "*".  

**Example 2:**
> **Input:** s = "1*"  
> **Output:** 18  
> **Explanation:** The encoded message can represent any of the encoded messages "11", "12", "13", "14", "15", "16", "17", "18", or "19".  
> Each of these encoded messages have 2 ways to be decoded (e.g. "11" can be decoded to "AA" or "K").  
> Hence, there are a total of 9 * 2 = 18 ways to decode "1*".

**Example 3:**
> **Input:** s = "2*"  
> **Output:** 15  
> **Explanation:**  
> The encoded message can represent any of the encoded messages "21", "22","23", "24", "25", "26", "27", "28", or "29".  
> "21", "22", "23", "24", "25", and "26" have 2 ways of being decoded, but "27", "28", and"29" only have 1 way.  
> Hence, there are a total of (6 * 2) + (3 * 1) = 12 + 3 = 15 ways to decode "2*".

**Constraints:**
* `1 <= s.length <= 10^5`
* `s[i]` is a digit or `'*'`.
### Analysis
Here is a random example to demonstrate the decode process:
```text
indices: 0 1 2 3 4 5 6 7   
s:       3 2 9 * 8 1 4 2
```
First, The numbers `7, 8, 9` can only be decoded as a single number, 
and the numbers `1-6` can be decoded as a single number or when the previous number is `1` or `2`,
they can be decoded together with the previous number as a new number.
### Dynamic Programming Solution
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

#### Initialization 
Define a one-dimensional array `dp`, where `dp[i]` represents the number of ways to decode the  the string `s` from index `0` to index `i-1`.
The length of the `dp` array is `s.length+1`, providing space to prevent missing the value of `dp[i-2]`.
####  Filling the DP Table  
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
#### Result
The `dp[len]` is the number of ways to decode the string `s`.

#### Implementation
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
#### Time and Space Complexity
* Time Complexity: $ O(n) $

    The `for` loop runs from `𝑖=2` to `𝑖=𝑙𝑒𝑛`, where `len` is the length of the string `s`, resulting in a time complexity of $O(n)$
* Space Complexity: $ O(n) $

    The `dp` array is of size `𝑛+1`, where `𝑛` is the length of the string.
    Thus, the overall space complexity is $O(n)$.
#### Consideration
* Calculate the single-digit decoding cases first to avoid redundant calculations.
* The time complexity of `s.toCharArray()` is O(n), while the `s.charAt()` has a time complexity of O(1), making `charAt()` more efficient.
* The values in the `dp` array are taken modulo 10<sup>9</sup> + 7, which is still a very large value. long type is required to prevent integer overflow.
* Using `Character.getNumericValue()` to obtain the nummeric value of `'*'` in string `s` will return `'-1'` and using the `-1` for checking purposes can lead to misleading readability.
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
* `1 <= receiver.length == n <= 10^5`

* `0 <= receiver[i] <= n - 1`

* `1 <= k <= 10^10`
### Analysis
Using simple enumeration, we can calculate the sum at each index and compare them to determine the maximum sum.  
However, calculating for each index involves a significant amount of repeat computation, resulting in very low performance, we need to minimize the number of passes and avoid redundant calculations.
### Dynamic Programming Solution

Define a two-dimensional array `pa[i][x]` to store the receiver value `x` reached from the initial receiver after $ 2^i $ passings.
Initially, `pa[0][x]` is simply the direct receiver of `x`. 

Similarly, define a two-dimensional array `sum[i][x]` to store the cumulative sum of receiver values when making $ 2^i $ passings from receiver `x`.   
`sum[0][x]` is simply the receiver value at `receiver[x]`, as it represents a single passing.

We can pass the ball to a distant receiver by skipping $ 2^n $ passings, 
instead of passing to the next receiver, as we have stored the cumulative sums for each passing from from each receiver.

#### Precomputation Process
Since each passing distance doubles the previous one,
the current receiver can be determined based on the previously calculated results for the current passing distance.
```text
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0]  
 1       ->  5    ->     9
 1                ->     9
```
We can determine the receiver as follows:  
Receiver `5` is reached after $2^2$ passes from receiver `1`.  
Receiver `9` is reached after $2^2$ passes from receiver `5`.  
Receiver `9` is also reached after $2^3$ passes from receiver `1`.

Here is a simple example to demonstrate the Precomputation process:
```text
receivers:  [1,2,3,4,5,6,7,8,9,10,0]  

Step 1:  
pa[0][1] = 2
pa[0][2] = 3
pa[0][3] = 4
pa[0][4] = 5
...

Step 2: 
pa[1][1] = pa[0][ pa[0][1] ]  = pa[0][2] = 3
pa[1][2] = pa[0][ pa[0][2] ]  = pa[0][3] = 4
pa[1][3] = pa[0][ pa[0][3] ]  = pa[0][4] = 5
...

Step 3: 
pa[2][1] = pa[1][ pa[1][1] ]  = pa[1][3] = 5
...
```
Since `pa[i][x]` represents the receiver after $2^i$ passes from receiver `x`, we can substitute this into the calculation for `pa[i+1][x]`:
$$ pa[i+1][x]=pa[i][pa[i][x]] $$

This relation allows us to efficiently compute the receiver after $2^{i+1}$ passes using previously calculated results.

As initialization, we can directly determine the receiver for each element after $2^0$ pass, which represents the immediate next receiver in a single pass.
#### Passing Process
Using bitwise operations can significantly improve the efficiency of the passing process:
* `k & k -1`  

    The operation clears the rightmost set bit (1) in the binary representation of k.
    
    It can result in:
    * Clearing the rightmost set bit (1) in the binary representation of `k`.  

        The operation `k & k -1` is equivalent to subtracting $2^n$ from `k`, where $n$ is the position of the rightmost set bit.
        For example, when comparing `24` and `12`, the number subtracted from `24` is greater than the number subtracted from `12`, because the rightmost set bit in `24` is further to the left than in `12`.

    * Clearing all bits when `k` is a power of two (like `8`, `16`, `32`, etc.)  

        When `k` is a power of two, `k & k -1` will return `0`.
    
    Example 1:  
    * $ k = 13 $ (binary: `1101`)
    * $ k-1=12 $ (binary: `1100`) 
    * `k & k-1` = `1101 & 1100 = 1100 ` = `12`

    Example 2:  
    * $ k = 24  $ (binary: `11000`)  
    * $ k-1=23 $ (binary: `10111`)  
    * `k & k-1` = `11000 & 10011 = 10000 ` = `16`

   Example 3:  
    * $ k = 12 $ (binary: `1100`)
    * $ k-1=11 $ (binary: `1011`) 
    * `k & k-1` = `1100 & 1011 = 1000 ` = `8`

    Example 4:
    * $ k = 8 $ (binary: `1000`)
    * $ k-1=7 $ (binary: `0111`) 
    * `k & k-1` = `1000 & 0111 = 0000 ` = `0`

* `64 - Long.numberOfLeadingZeros(k)`

    Java `long` values are represented using `64` bits.
    By subtracting the number of leading zeros from `64`,
    we determine the number of bits required to represent `k` in binary (its bit length),
    which also serves as **the exponent** of the power of two closest to k.

* `Long.numberOfTrailingZeros(k)`

    Count the trailing zeros of `k`, which corresponds to the exponent of the number $2^n$  substracted from `k` after the oprations `k &= k-1`.

#### Implementation
```java
class Solution {
    public long getMaxFunctionValue(List<Integer> receiver, long K) {
        int len = receiver.size();
        // The number of passes, which corresponds to the exponent of the power of 2 closest to k.
        int passCount = 64 - Long.numberOfLeadingZeros(K); 
        var pa = new int[passCount][len];
        var sum = new long[passCount][len];
        // Populate the direct receivers
        for (int i = 0; i < len; i++) {
            pa[0][i] = receiver.get(i);
            sum[0][i] = receiver.get(i);
        }
        // Precompute the sum starting from each index incrementally, step by step.
        for (int i = 0; i < passCount - 1; i++) {
            // Traverse receivers
            for (int x = 0; x < len; x++) {
                //  Get the receiver reached after 2^i passes from receiver x
                int p = pa[i][x];
                //  Get the receiver reached after 2^i passes from receiver p
                pa[i + 1][x] = pa[i][p];
                sum[i + 1][x] = sum[i][x] + sum[i][p];
            }
        }
        long ans = 0;
        // Pass the ball
        for (int i = 0; i < len; i++) {
            long s = i;
            int x = i;
            for (long k = K; k > 0; k &= k - 1) {
                // Count trailing zero
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
#### Time and Space Complexity
* Time Complexity: $ O(n \log k) $
    * Precomputation

        The outer loop takes $O(log k)$ time, as it calcualtes the powers of 2 up to $k$.
        The inner loop iterates $O(n)$ times for each outer loop iteration, calculating the receiver and sum for each index, where `n` is the length of the receiver array.

        So, the precomputation step takes $O(n \times log k)$ time.
    * Pass the ball  
        The outer loop iterates $O(n)$ times to consider each starting index.

        The inner loop iterates at most $O(log k)$ times to calculate the final sum for each starting index, using the precomputed values.

    Therefore, the overall time complexity of the algorithm is $ O(n \log k) $

* Space Complexity: $ O(n \log k) $
    * `pa` and `sum` arrays 
        
        These two arrays store information for each power of `2` up to `k`, and each entry in the array corresponds to a receiver, So the space complexity of these arrays is $ O(n \log k) $.

    * Other variables
        
        The other variables, such as `len`, `passCount`, `i`, `x`, `k`, `ctz`, `s`, and `ans`, require constant extra space.

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
> **Input:** piles = [5,3,4,5]  
> **Output:** true  
> **Explanation:**  
    Alice starts first, and can only take the first 5 or the last 5.  
    Say she takes the first 5, so that the row becomes [3, 4, 5].  
    If Bob takes 3, then the board is [4, 5], and Alice takes 5 to win with 10 points.  
    If Bob takes the last 5, then the board is [3, 4], and Alice takes 4 to win with 9 points.  
    This demonstrated that taking the first 5 was a winning move for Alice, so we return true.

**Example 2:** 
> **Input:** piles = [3,7,2,3]  
> **Output:** true

**Constraints:**
* `2 <= piles.length <= 500`
* `piles.length` is even.
* `1 <= piles[i] <= 500`
* `sum(piles[i])` is odd.
### Depth-first Search Solution
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
    if(left >= right)return false;
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
#### Time and Space Complexity
* Time Complexity: $O(2^n)$

    Alice and Bob can make two possible choices at each step, and the total number of choices is `right-left`,
    Therefore, the overall time complexity is $O(2^n)$.
    
* Space Complexity: $ O(n) $ 

    The depth of the recursive call stack is `right-left`, representing the number of choices for Alice and Bob,
    resulting in a total space complexity of $O(n)$.



### Dynamic Programming Solution
#### Initialization  
Define a two-dimensional array `dp`, where both the rows and columns correspond to the indices of the array `piles`.  
The `dp[i][j]` represents the difference between the number of stones Alice has and the number of stones Bob has, when considering the subarray from index `i` to `j` of the piles array.  
When `i` equals `j`, there is only a single pile of stones, which is `piles[i]`.
Since Alice goes first, she takes this pile, so `dp[i][j]=piles[i]`.
####  Filling the DP Table  
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
####  Result  
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
#### Time and Space Complexity
* Time Complexity: $ O(n^2)$ 

    The sum of iterations for both loop is:  
    $$ 1+2+3+...+(n-1) = \frac{(n-1) \times n}{2} =  O(n^2)$$
* Space Complexity: $ O(n^2)$ 

    The algorithm uses a two-dimensional array `dp` of size `n×n`, where `n` is the length of the input array `piles`.
    The space required for this array is $ O(n^2)$ .
### Optimized Dynamic Programming Solution
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
#### Time and Space Complexity
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

**Example 1:**  
> **Input:** nums = [1,1,1,1,1], target = 3  
> **Output:** 5  
> **Explanation:**  
    There are 5 ways to assign symbols to make the sum of nums be target 3.  
    -1 + 1 + 1 + 1 + 1 = 3  
    +1 - 1 + 1 + 1 + 1 = 3  
    +1 + 1 - 1 + 1 + 1 = 3  
    +1 + 1 + 1 - 1 + 1 = 3  
    +1 + 1 + 1 + 1 - 1 = 3  

**Example 2:**
> **Input:** nums = [1], target = 1  
> **Output:** 1

**Constraints:**
* `1 <= nums.length <= 20`
* `0 <= nums[i] <= 1000`
* `0 <= sum(nums[i]) <= 1000`
* `-1000 <= target <= 1000`
### Depth-first Search Solution
Each element of the array nums can be added either a `+` or `-` sign, 
resulting in `2` choices per element and a total of $2^n$ combinations for `n` elements.  
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
#### Time and Space Complexity
* Time Complexity: $ O(2^n)$ 

    The total number of recursive calls is proportional to $2^n$, 
    as each element can either contribute positively or negatively to the sum.
* Space Complexity: $ O(n) $ (for the recursion stack)
### Optimized Depth-first Search Solution
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
#### Implementation
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
#### Time and Space Complexity
* Time Complexity: $ O(2^n)$  
    
    The worst-case time complexity remains the same, but it is faster than the original solution in general cases.
* Space Complexity: $ O(n \times neg) $ 
    * Recursive call stack takes $O(n)$ space.
    * For the memoization map, the key takes `n` possible values (`0` to `n-1`).
     the value can range from `0` to the value of `neg`, which is `(sum-target)-2`.  
     Hence, the map `memo` takes a time complexity $O(n \times neg)$.

### Dynamic Programming Solution
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
        // Iterate through the array 'nums' and compute all possible DP results for each index.
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
#### Time and Space Complexity
* Time Complexity: $ O(n \times neg) $ 
    * Calculate the sum of nums.

        Traversing array `nums` takes $O(n)$ time.
    * Iterate through the array 'nums' and compute all possible DP results for each index.

        The combined time complexity for the outer and inner `for` loop is $O(n \times neg)$.

     Therefore, the total time complexity is $O(n \times neg)$.

* Space Complexity: $ O(n \times neg) $

    The `dp` array requires $O(n \times neg)$ space.

### Optimized Dynamic Programming Solution

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
#### Time and Space Complexity
* Time Complexity: $ O(n×neg) $ 
    * Calculate the sum of nums.

        Traversing array `nums` takes $O(n)$ time.
    * Prepare DP results for subsequent traversal.

        The combined time complexity for the outer and inner `for` loop is $O(n \times neg)$.

     Therefore, the total time complexity is $O(n \times neg)$.
* Space Complexity: $ O(neg) $

    The `dp` array requires $O(neg)$ space.

# Math
## Construct the Minimum Bitwise Array II
### Overview
You are given an array `nums` consisting of `n` prime integers.

You need to construct an array `ans` of length `n`, such that, for each index `i`, the bitwise `OR` of `ans[i]` and `ans[i] + 1` is equal to `nums[i]`, i.e. `ans[i] OR (ans[i] + 1) == nums[i]`.

Additionally, you must minimize each value of `ans[i]` in the resulting array.

If it is not possible to find such a value for `ans[i]` that satisfies the condition, then set `ans[i] = -1`.

**Example 1:**

> **Input:** nums = [2,3,5,7]  
> **Output:** [-1,1,4,3]  
> **Explanation:**
> * For `i = 0`, as there is no value for `ans[0]` that satisfies `ans[0] OR (ans[0] + 1) = 2`, so `ans[0] = -1`.
> * For `i = 1`, the smallest `ans[1]` that satisfies `ans[1] OR (ans[1] + 1) = 3` is `1`, because `1 OR (1 + 1) = 3`.
> * For `i = 2`, the smallest `ans[2]` that satisfies `ans[2] OR (ans[2] + 1) = 5` is `4`, because `4 OR (4 + 1) = 5`.
> * For `i = 3`, the smallest `ans[3]` that satisfies `ans[3] OR (ans[3] + 1) = 7` is `3`, because `3 OR (3 + 1) = 7`.

**Example 2:**
> **Input:** nums = [11,13,31]  
> **Output:** [9,12,15]  
> **Explanation:**  
> * For `i = 0`, the smallest `ans[0]` that satisfies `ans[0] OR (ans[0] + 1) = 11` is `9`, because `9 OR (9 + 1) = 11`.
> * For `i = 1`, the smallest `ans[1]` that satisfies `ans[1] OR (ans[1] + 1) = 13` is `12`, because `12 OR (12 + 1) = 13`.
> * For `i = 2`, the smallest `ans[2]` that satisfies `ans[2] OR (ans[2] + 1) = 31` is `15`, because `15 OR (15 + 1) = 31`.

**Constraints:**
* `1 <= nums.length <= 100`
* `2 <= nums[i] <= 10^9`
* `nums[i]` is a prime number.

### Analysis
Here are several examples:
```text
0100 | 0101 = 0101          4 | 5 = 5     
0010 | 0011 = 0011          2 | 3 = 3     
10101 | 10110 = 10111       21 | 22 = 23  

0001 | 0010 = 0011          1 | 2 = 3   
1001 | 1010 = 1011          9 | 10 = 11   
0011 | 0100 = 0111          3 | 4 = 7    
0111 | 1000 = 1111          7 | 8 = 15   
100111 | 101000 = 101111    39 | 40 = 47 
```
This problem can be divided into two cases:
* `nums[i]` is an even number.

    It is impossible to find a value for `ans[i]` that satisfies the condition `ans[i] | (ans[i]+1) = nums[i]`.
    This is because for even numbers, the rightmost bit of `ans[i]` will always flip when adding `1`, resulting in an odd number.
    Therefore, no solution exists, and the result should be `-1`.

* `nums[i]` is an odd number.

    If `nums[i]` has a continuous sequence of rightmost `1` bits, a minimal value for `ans[i]` can be achieved by setting the highest bit in this sequence to `0`.
    
    The simplest case is when `ans[i] = nums[i]-1`. In this case, the condition is satisfied by flipping the rightmost bit of `ans[i]`.    
    
    When `1` is added to `ans[i]`, a carry propagates through the sequence, restoring the higher bit to `1` and satisfying the condition.  
    For example:
    ```text
    101110 | 101111 = 101111    46 | 47 = 47 
    100111 | 101000 = 101111    39 | 40 = 47 

    (47 + 1) & ~47 = 0011 0000 & 1101 0000 = 0001 0000
    0001 0000 >> 1 = 0000 1000
    0010 1111 ^ 0000 1000 = 0010 0111
    ```
    In these cases, the continuous rightmost sequence of `45` in `101111` is `1111`. 
    By manipulating the bits as described, the minimal value of `ans[i]` can be obtained.
        
### Implementation
```java
class Solution {
    public int[] minBitwiseArray(List<Integer> nums) {
        int[] ans=new int[nums.size()];
        for(int i=0; i<nums.size(); i++){
            int cu=nums.get(i);
            if(cu%2==0){
                ans[i]=-1;
            }else{
                // Set the highest bit in the continuous sequence of rightmost `1` bits to `0`
                // (47 + 1) & ~47 = 0011 0000 & 1101 0000 = 0001 0000
                // 0001 0000 >> 1 = 0000 1000
                // 0010 1111 ^ 0000 1000 = 0010 0111
                int hb = (cu + 1) & ~cu;
                ans[i] = cu ^ (hb >> 1);
            }
        }
        return ans;
    }
}
```
#### Time and Space Complexity
* Time Complexity: $ O(n) $  

    The `for` loop takes $O(n)$ time.
* Space Complexity: $ O(n) $

    The array `ans` takes $O(n)$ space.

## Find Number of Ways to Reach the K-th Stair
[Back to Top](#table-of-contents)  
### Overview
You are given a **non-negative** integer `k`. There exists a staircase with an infinite number of stairs, with the **lowest** stair numbered 0.

Alice has an integer `jump`, with an initial value of `0`. 
She starts on stair 1 and wants to reach stair `k` using any number of operations. 
If she is on stair `i`, in one operation she can:

Go down to stair `i - 1`. This operation cannot be used consecutively or on stair 0.
Go up to stair $i + 2^{\text{jump}} $. And then, `jump` becomes `jump + 1`.
Return the total number of ways Alice can reach stair `k`.

Note that it is possible that Alice reaches the stair `k`, and performs some operations to reach the stair `k` again.

**Example 1:**
> **Input:** k = 0  
> **Output:** 2  
> **Explanation:**  
> The 2 possible ways of reaching stair 0 are:
> * Alice starts at stair 1.
>   * Using an operation of the first type, she goes down 1 stair to reach stair 0.
> * Alice starts at stair 1.
>   * Using an operation of the first type, she goes down 1 stair to reach stair 0.
>   * Using an operation of the second type, she goes up 20 stairs to reach stair 1.
>   * Using an operation of the first type, she goes down 1 stair to reach stair 0.

**Example 2:**
> **Input:** k = 1  
> **Output:** 4  
> **Explanation:**  
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
* `0 <= k <= 10^9`

### Analysis
Based on the formula for the sum of a geometric series:
$$ 2^0 + 2^1 + 2^2 + ... + 2^n = 2^{n+1}-1$$
If Alice reaches the stair `k` after `e` upward jumps and `f` downward jumps, then:
$$ 2^0 + 2^1 + 2^2 + ... + 2^{e-1} = 2^e-1$$
Thus, the relationship is:
$$ 1 + ( 2^e-1 )- f = k $$
Rearranging gives:
$$ f = 2^e - k$$ 
Since there are `e+1` positions where these downward jumps can occur, the result is:
$$ C(e+1, 2^e - k) $$
Probability Formula:
* Order doesn't matter (Combinations):
    $$ C(n,m)= \frac{n!}{m! \times (n-m)!} $$
* Order matters (Permutations)
    $$ P(n,m)= \frac{n!}{(n-m)!} $$
    
    * **n**: Total number of elements in the set.  
    * **m**: Number of elements to choose.  
    * **!**: Factorial (e.g. $5!=5 \times 4 \times 3 \times 2 \times 1 $)

Using `Integer.highestOneBit(k)` to determine the nearest lower power of two (`nlp`) for `k`, 
consider the following cases:
* If `k=0`, no jumps are required.
* If `nlp=k`, there is one valid case where all jumps are upward.
* If `nlp<k`, an additional upward jump is needed to pass stair `k`, followed by several downward jumps to return to `k`.
#### Precomputation for Combination Probability
Using the following combination formula, the results can be precomputed and stored in a two-dimensional array:
$$C(n,m)=C(n-1,m-1)+C(n-1,m)$$

#### Evaluating the Value Range of Combination Probability
Binomial Theorem:
$$ (a + b)^n = \sum_{k=0}^n C(n, k) \times a^{n-k} \times b^k $$
For a=1, b=1, the result becomes:
$$ (1 + 1)^n = \sum_{k=0}^n C(n, k) = C(n,0)+C(n,1)+C(n,2)+...+C(n,n) $$
Since $C(n,k)$ is one of these terms, it follows that: 
$$ C(n,k)<2^n $$

Let the exponent of the nearest lower power of two for `k` be `ex`,  
Given the constraint $0 <= k <= 10^9$, 
even though an additional upward jump may need to be considered,
The combination probability result remains less than $2^{ex+1}$, which is equivalent to `2k`.  
Since integer value range is $-2,147,483,648$ to $2,147,483,647$,the result within $0 \sim 2\times10^9$ can be safely represented as an `int`.
#### Implementation
```java
class Solution {
    private static final int MX = 31;
    private static final int[][] c = new int[MX][MX];

    static {
        for (int n = 0; n < MX; n++) {
            c[n][0] = c[n][n] = 1;
            for (int m = 1; m < n; m++) {
                c[n][m] = c[n - 1][m - 1] + c[n - 1][m];
            }
        }
    }

    public int waysToReachStair(int k) {
        // The nearest power of two for k.
        int nlp=Integer.highestOneBit(k);
        // The exponent of the nearest power of two for k.
        int ex=32 - Integer.numberOfLeadingZeros(k);
        int result=0;
        if(k==1) result++;
        if(nlp==k) result++;
        if((nlp<<=1)-k <= ex+1){
            result+=c[ex+1][nlp-k];
        }
        return result;
    }
}
```
#### Time and Space Complexity
* Time Complexity: $ O(1) $
 
    The time and space used duiring the precomputation process are not factored into the solution.
* Space Complexity: $ O(1) $
## Minimum Moves to Capture The Queen
[Back to Top](#table-of-contents)  
### Overview
There is a **1-indexed** `8 x 8` chessboard containing 3 pieces.

You are given `6` integers `a`, `b`, `c`, `d`, `e`, and `f` where:

* `(a, b)` denotes the position of the white rook.
* `(c, d)` denotes the position of the white bishop.
* `(e, f)` denotes the position of the black queen.
Given that you can only move the white pieces, return the **minimum** number of moves required to capture the black queen.

**Note** that:

* Rooks can move any number of squares either vertically or horizontally, but cannot jump over other pieces. 
* Bishops can move any number of squares diagonally, but cannot jump over other pieces. 
* A rook or a bishop can capture the queen if it is located in a square that they can move to. 
* The queen does not move. 

**Example 1:**   
![mmtctq1](assets/Algorithms/mmtctq1.png)

> **Input:** a = 1, b = 1, c = 8, d = 8, e = 2, f = 3  
> **Output:** 2  
> **Explanation:** We can capture the black queen in two moves by moving the white rook to (1, 3) then to (2, 3).  
> It is impossible to capture the black queen in less than two moves since it is not being attacked by any of the pieces at the beginning.  

**Example 2:**  
![mmtctq2](assets/Algorithms/mmtctq2.png)
> **Input:** a = 5, b = 3, c = 3, d = 4, e = 5, f = 2  
> **Output:** 1  
> **Explanation:** We can capture the black queen in a single move by doing one of the following: 
> - Move the white rook to (5, 2).
> - Move the white bishop to (5, 2).

**Constraints:**
* `1 <= a, b, c, d, e, f <= 8`
* No two pieces are on the same square.

### Analysis
Here are the coordinates of the pieces:
```text
Rook:   (a,b)
Bishop: (c,d)
Queen:  (e,f)
```
Cases where the rook can directly capture the queen:
* rook and queen in the same row (bishop is not in between):   

    `c != a`: The bishop is not in the same row as the rook.  
    `d <= Math.min(b, f)`: The bishop is to the left of both the rook and the queen.    
    `d >= Math.min(b, f)`: The bishop is to the right of both the rook and the queen.  
* rook and queen in the same column (bishop is not in between):  

    `d != b`:  The bishop is not in the same column as the rook.  
    `c <= Math.min(a, e)`: The bishop is below both the rook and the queen.  
    `c >= Math.min(a, e)`: The bishop is above both the rook and the queen.

Cases where the bishop can capture the queen:
* The bishop and queen on the same diagonal (rook is not in between).  

    Using the equation of a line $ y = ax + b $ (where $a=1$ since the line is diagonal in this problem),
    two points $(x_1, y_1)$ and $(x_2, y_2)$ to lie on the same diagonal of a coordinate grid if one of the following conditions is met:

    * They lie on the main diagonal where $ x_1 - x_2 = y_1-y_2 $  (difference of coordinates is equal)
    * They lie on the anti-diagonal where $ x_1 - x_2 = -(y_1-y_2) $  (sum of coordinates is constant)  

    Combining these two conditions into one equation:
        $$ | x_1 - x_2 |  = | y_1-y_2 | $$

    To determine if the bishop, queen, and rook are collinear, compare the slopes of the lines formed by these points:
    $$ \frac{y_3-y_1}{x_3-x_1}=\frac{y_2-y_1}{x_2-x_1} $$
    If this equation holds, the three pieces are collinear; otherwise, they are not. To avoid division, the equation can be rewritten as:
    $$ (y_3-y_1)\times(x_2-x_1)=(y_2-y_1)\times(x_3-x_1) $$

Other cases:
* If a piece blocks the direct capture path, 
it can either be moved or an additional step can be taken to bypass the obstruction and approach the queen. 
In such situations, the maximum steps required are 2.
* If the rook or bishop cannot directly capture the queen, they can move an additional step to do so.
The maximum steps required are also 2.

#### Implementation
```java
class Solution {
    private int moves=0;
    public int minMovesToCaptureTheQueen(int a, int b, int c, int d, int e, int f) {
        // Rook:    (a,b)
        // Bishop:  (c,d)
        // Queen:   (e,f)

        // The rook and queen in the same row (bishop is not in between)
         if (a == e && (c != a || d <= Math.min(b, f) || d >= Math.max(b, f))) {
            return 1;
        }
        // The rook and queen in the same column (bishop is not in between)
        if (b == f && (d != b || c <= Math.min(a, e) || c >= Math.max(a, e))) {
            return 1;
        }
        // The bishop and queen on the same diagonal (rook is not in between)
       if (Math.abs(c - e) == Math.abs(d - f) 
            && ((c - e) * (b - f) != (a - e) * (d - f) 
            || a < Math.min(c, e) || a > Math.max(c, e))) {
            return 1;
        }
        return 2;
    }
}
```
#### Time and Space Complexity
* Time Complexity: $ O(1) $
* Space Complexity: $ O(1) $
# Precomputation
## Range Product Queries of Powers
[Back to Top](#table-of-contents)  
### Overview
Given a positive integer `n`, there exists a **0-indexed** array called `powers`, composed of the **minimum** number of powers of `2` that sum to `n`. The array is sorted in **non-decreasing** order, and there is only one way to form the array.

You are also given a **0-indexed** 2D integer array `queries`, where `queries[i] = [lefti, righti]`. Each `queries[i]` represents a query where you have to find the product of all `powers[j]` with $ left_i <= j <= right_i $.

Return an array `answers`, equal in length to `queries`, where `answers[i]` is the answer to the $i^{th}$ query. Since the answer to the $i^{th}$  query may be too large, each `answers[i]`should be returned modulo $10^9 + 7$.

**Example 1:**
> **Input:** n = 15, queries = [[0,1],[2,2],[0,3]]  
> **Output:** [2,4,64]  
> **Explanation:**  
> For n = 15, powers = [1,2,4,8]. It can be shown that powers cannot be a smaller size.  
> Answer to 1st query: powers[0] * powers[1] = 1 * 2 = 2.  
> Answer to 2nd query: powers[2] = 4.  
> Answer to 3rd query: powers[0] * powers[1] * powers[2] * powers[3] = 1 * 2 * 4 * 8 = 64.  
> Each answer modulo $10^9 + 7$ yields the same answer, so [2,4,64] is returned.

**Example 2:**
> **Input:** n = 2, queries = [[0,0]]  
> **Output:** [2]  
> **Explanation:**  
> For n = 2, powers = [2].  
> The answer to the only query is powers[0] = 2. The answer modulo $10^9 + 7$ is the same, so [2] is returned.

**Constraints:**
* `1 <= n <= 10^9`
* `1 <= queries.length <= 10^5`
* `0 <= start_i <= end_i < powers.length`

### Analysis
In the problem description, "the minimum number of powers of 2 that sum to n" corresponds to the number of set bits in the binary representation of the integer `n`.

Example:
```text
40 = 0010 1000 (two's complement) = 32 + 8
```
The two set bits represent the desired result,
with each set bit's value added to the array `powers`.
Using a mask value of `0001` and shift it left to isolate the set bits.

Since the array `powers` is small (<32), precomputing all products for possible query ranges ensures efficiency with small time complexity.
#### Implementation
```java
class Solution {
    int MOD=1_000_000_007;
    public int[] productQueries(int n, int[][] queries) {
        int bc=Integer.bitCount(n);
        int[] powers=new int[bc];        
        // Populate these powers of two into array powers
        for(int i=1,j=0; j<bc; i<<=1){
            if((i&n)==i){
                powers[j]=i;
                j++;
            }
        }

        // Precompute the product results for all subarrays
        int[][] productRes = new int[bc][bc];
        for (int i = 0; i < bc; i++) {
            productRes[i][i] = powers[i]; 
            for (int j = i + 1; j < bc; j++) {
                productRes[i][j] = (int) ((long) productRes[i][j - 1] * powers[j] % MOD);
            }
        }

        // Answer each query
        int[] output=new int[queries.length];
        for(int i=0; i<queries.length; i++){
            int l = queries[i][0];
            int r = queries[i][1];
            output[i] = productRes[l][r];
        }
        return output;
    }
}
```
#### Time and Space Complexity
* Time Complexity: $O(bc^2)$
    * Integer.bitCount(n)
    
        Counts the number of set bits in the binary representation of integer `n`. This runs in $O(1)$ time.

    * Populate these powers of two into array powers
        
        The `for` runs `bc` times, resulting a time complexity $O(bc)$.

    * Precompute the product results for all subarrays
        
        The outer `for` loop runs `bc` times, and the number of iterations times of the inner `for` loop depends on the outer loop, so the total number of runs is:
        $$ \sum_{i=0}^{bc-1} (bc-1-i) = (bc-1) + (bc-2) + (bc-3) + ... + 1 = \frac{(bc-1)\times bc}{2} $$
        Therefore, the time complexity is $ O(bc^2) $. 

    * Answer each query

        The `for` loop runs `queries.length` times, so the time complexity is $O(q)$ for `q` queries.

     Since $O(q)$ grows slower than O(bc^2) and can be omitted, the total time compleixty is $O(bc^2)$

* Space Complexity: $O(bc^2)$
    * `powers` array takes $O(bc)$ space.  
    * `productRes ` array takes $O(bc^2)$ space.  
    * `output ` array takes $O(q)$ space.

    Because $O(q)$ grows slower than $O(bc^2)$ and can be omitted, the total space complexity is $O(bc^2)$.

# Sliding Window
## Find the Longest Equal Subarray
[Back to Top](#table-of-contents)  
### Overview
You are given a **0-indexed** integer array nums and an integer `k`.  
A subarray is called **equal** if all of its elements are equal. Note that the empty subarray is an **equal** subarray.  
Return the length of the **longest** possible equal subarray after deleting **at most** `k` elements from `nums`.  
A **subarray** is a contiguous, possibly empty sequence of elements within an array.

**Example 1:**
> **Input:** nums = [1,3,2,3,1,3], k = 3  
> **Output:** 3  
> **Explanation:**   
> It's optimal to delete the elements at index 2 and index 4.  
> After deleting them, nums becomes equal to [1, 3, 3, 3].  
> The longest equal subarray starts at i = 1 and ends at j = 3 with length equal to 3.  
> It can be proven that no longer equal subarrays can be created.

**Example 2:**
> **Input:** nums = [1,1,2,2,1,1], k = 2  
> **Output:** 4  
> **Explanation:**   
> It's optimal to delete the elements at index 2 and index 3.  
> After deleting them, nums becomes equal to [1, 1, 1, 1].  
> The array itself is an equal subarray, so the answer is 4.  
> It can be proven that no longer equal subarrays can be created.

**Constraints:**  
* `1 <= nums.length <= 10^5`
* `1 <= nums[i] <= nums.length`
* `0 <= k <= nums.length`
### Analysis
Finding the longest possible equal subarray involves counting the number of identical numbers.   
Since we can delete at most `k` elements, 
we need to count the number of unequal numbers between the identical numbers to determine how many deletions are required.

Assuming an integer `m` exists at index `L` and within the range from `L` to `R`, it is used for comparision.
Move the range form left to right in the array `nums` as a sliding window.

During this process, the following operations are needed:
* If the count of the unequal numbers for `m` in this range is less than `k`, we can directly count the occurrences of `m` as a valid result if it is the maximum.
* Otherwise, the `m` at index `L` will be excluded from this range, as the count of unequal numbers exceeds the restriction `k`, even if additional integers with the value `m` exist beyond index `R`.
  Keep moving the sliding window to the right by increasing right boundary index `R`.

There are several different choices next:
* Group each value and store the indexes of integers with the same value, then repeat the above process for each index group.
* Continue excluding other integers after excluding the `m` at index `L` to directly shrink the sliding window,
  and check whether the occurrences of each excluded integer are the maximum in the current range.
  keeping the right boundary index `R` constant during the process, and resume moving the sliding window to the right by shifting right boundary `R`.   

  Below is an example for the exclusion process when the occurrences of unequal integer exceed the restriction `k`: 
  ```text
  nums: 1,3,4,1,2,9,6,7,2,3
  k: 3

  Step 1: 
  1,3,4,1,2,9,6,7,2,3
  L         R

  Step 2:
  1,3,4,1,2,9,6,7,2,3
    L       R
  
  Step 3: 
  1,3,4,1,2,9,6,7,2,3
      L     R
  ```
  Since there are 4 unequal integers in the range `L` to `R` for integer `1` at the left boundary, the execlusion process starts.
  
  The integer `1` and `3` are execluded, and the `4` becomes the new comparision number.
  Both `1` and `3` must be checked whether their occurrences are the maximum in the current range before the exclusion.

  By counting the occurrences of each value in the current range, we can ensure each number to be excluded is properly checked.  

Here, choose the second one as it uses less space.
#### Implementation
```java
class Solution {
  public int longestEqualSubarray(List<Integer> nums, int k) {
    // Define an array to store the quantity of equal values. If the values in the array nums are large, use HashMap.
    int[] frequency = new int[nums.size() + 1];
    int L = 0;
    // The final result
    int res = 0; 
    // The count of numbers not equals to the integer at index left within the range from left (inclusive) to index i (inclusive).
    int neq = 0; 
    // Traverse array nums
    for (int R = 0; R < nums.size(); R++) {
      // Count the number of times the current value occurs.
      ++frequency[nums.get(R)];
      // Count the number of interges not equals to the interger at index left.
      if (!Objects.equals(nums.get(R), nums.get(L))) {
        neq++;
      }
      // When the count of unequal numbers exceeds the restriction k, exclude numbers to shrink the sliding window. 
      while (neq > k) {
        frequency[nums.get(L)]--;
        L++;
        // The count of unqeual numbers equals to the total count minus the count of equal numbers.
        neq = R - L + 1 - frequency[nums.get(L)];
      }
      res = Math.max(res, R - L + 1 - neq);
    }

    // Traverse remaining integers
    while (L < nums.size() - 1) {
      frequency[nums.get(L)]--;
      L++;
      res = Math.max(res, frequency[nums.get(L)]);
    }
    return res;
  }
}
```
#### Time and Space Complexity
* Time Complexity: $ O(n) $  
    * Traverse array nums
    
        The outer `for` loop iterates over each element of the `nums` array once, making the time complexity $ O(n) $.  

        While the inner while loop might seem to potentially iterate multiple times, its amortized time complexity is $ O(1) $. This is because each element is only removed from the window once.
    * Traverse remaining integers

        This loop only traverses the remaining integers after the sliding window reaches the end of the nums array, with a time complexity of $O(1)$ corresponding to the size of the sliding window.

    Therefore, the overall time complexity of the algorithm is $ O(n) $
* Space Complexity: $ O(n) $
    * valCount Array
    
        The valCount array has a size of `nums.size() + 1`. In the worst case, where the values in nums are distinct and large, the space required for valCount would be proportional to the size of nums, i.e., $ O(n) $.

    * Other Variables
    
        The rest of the variables (left, res, neq) are scalar integers, each taking constant space.
   
   Since the primary space usage comes from the `valCount` array, the space complexity is $ O(n) $

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
> **Input:** s = "5F3Z-2e-9-w", k = 4  
> **Output:** "5F3Z-2E9W"  
> **Explanation:** The string s has been split into two parts, each part has 4 characters.   
> Note that the two extra dashes are not needed and can be removed.

**Example 2:**
> **Input:** s = "2-5g-3-J", k = 2  
> **Output:** "2-5G-3J"  
> **Explanation:** The string s has been split into three parts, each part has 2 characters except the first part as it could be shorter as mentioned above.
 

**Constraints:**
* `1 <= s.length <= 10^5`
* `s` consists of English letters, digits, and dashes `'-'`.
* `1 <= k <= 10^4`
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
#### Time and Space Complexity
* Time Complexity: $ O(n) $

    The time complexity of methods `toUpperCase, replaceAll, toCharArray` are all $ O(n) $.  
    Additionally, the time complexity for `substring` is $O(m)$, where $m$ is the length of the substring (endIndex - startIndex).  
    Since each iteration takes $O(k)$ time where $k$ is the length of the sliced substring and there are `(n-firstLen)/k` iterations, the loop takes $ O(n) $.
    Therefore, the total time complexity is $ O(n) $.
* Space Complexity: $ O(n) $
    
    `s.toCharArray()` creates a new character array of size $ O(n) $,
    `StringBuilder sb` stores the result string, which can also be of size $ O(n) $.  
    Therefore, the total space complexity is $ O(n) $
# Traversal
## Maximum Number of Operations With the Same Score I
You are given an array of integers `nums`. Consider the following operation:

* Delete the first two elements `nums` and define the score of the operation as the sum of these two elements.
You can perform this operation until `nums` contains fewer than two elements. Additionally, the **same** score must be achieved in **all** operations.

Return the **maximum** number of operations you can perform.

**Example 1:**
> **Input:** nums = [3,2,1,4,5]  
> **Output:** 2  
> **Explanation:**  
> * We can perform the first operation with the score `3 + 2 = 5`. After this operation, `nums = [1,4,5]`.
> * We can perform the second operation as its score is `4 + 1 = 5`, the same as the previous operation. After this operation, `nums = [5]`.
> * As there are fewer than two elements, we can't perform more operations.

**Example 2:**
> **Input:** nums = [1,5,3,3,4,1,3,2,2,3]  
> **Output:** 2  
> **Explanation:**  
> * We can perform the first operation with the score 1 + 5 = 6. After this operation, nums = [3,3,4,1,3,2,2,3].
> * We can perform the second operation as its score is 3 + 3 = 6, the same as the previous operation. After this operation, nums = [4,1,3,2,2,3].
> * We cannot perform the next operation as its score is 4 + 1 = 5, which is different from the previous scores.

**Example 3:**
> **Input:** nums = [5,3]  
> **Output:** 1  

**Constraints:**
* `2 <= nums.length <= 100`
* `1 <= nums[i] <= 1000`

### Analysis
Since `2 <= nums.length <= 100`, the operation score can be determined using the first two elements of the array nums.   
Traverse the array `nums` to evaluate the maximum number of operations you can perform sequentially.
#### Implementation
```java
class Solution {
    public int maxOperations(int[] nums) {
        //2 <= nums.length <= 100
        int sum=nums[0]+nums[1];
        int nb=1;
        for(int i=2;i+1<nums.length; i+=2){
            if(nums[i]+nums[i+1]==sum)++nb;
            else break;
        }
        return nb;
    }
}
```
#### Time and Space Complexity
* Time Complexity: $ O(n) $

    The `for` loop iterates `nums.length/2 - 2` times, leading to a time complexity of $O(n)$.


* Space Complexity: $ O(1) $

    Only a constant amount of additional space is used.
# SQL
## Odd and Even Transactions
[Back to Top](#table-of-contents)  
### Overview
Table: `transactions`
```
+------------------+------+  
| Column Name      | Type |   
+------------------+------+  
| transaction_id   | int  |  
| amount           | int  |  
| transaction_date | date |  
+------------------+------+  
```
The `transactions_id` column uniquely identifies each row in this table.  
Each row of this table contains the transaction id, amount and transaction date.

Write a solution to find the `sum of amounts` for `odd` and `even` transactions for each day. If there are no odd or even transactions for a specific date, display as `0`.

Return the result table ordered by `transaction_date` in **ascending** order.
The result format is in the following example.

**Example:**
> **Input:**  
> `transactions` table:  
> ```text
> +----------------+--------+------------------+
> | transaction_id | amount | transaction_date |
> +----------------+--------+------------------+
> | 1              | 150    | 2024-07-01       |
> | 2              | 200    | 2024-07-01       |
> | 3              | 75     | 2024-07-01       |
> | 4              | 300    | 2024-07-02       |
> | 5              | 50     | 2024-07-02       |
> | 6              | 120    | 2024-07-03       |
> +----------------+--------+------------------+
> ```
> **Output:**  
> ```text
> +------------------+---------+----------+
> | transaction_date | odd_sum | even_sum |
> +------------------+---------+----------+
> | 2024-07-01       | 75      | 350      |
> | 2024-07-02       | 0       | 350      |
> | 2024-07-03       | 0       | 120      |
> +------------------+---------+----------+
> ```
> **Explanation:**
> * For transaction dates:
>   * 2024-07-01:
>       * Sum of amounts for odd transactions: 75
>       * Sum of amounts for even transactions: 150 + 200 = 350
>   * 2024-07-02:
>       * Sum of amounts for odd transactions: 0
>       * Sum of amounts for even transactions: 300 + 50 = 350
>   * 2024-07-03:
>       * Sum of amounts for odd transactions: 0
>       * Sum of amounts for even transactions: 120  
>
> **Note:** The output table is ordered by `transaction_date` in ascending order.
### Analysis
Group the data by the `transaction_date` field (using `GROUP BY` or `PARTITION BY`), and calculate the total amounts for odd and even transactions on each transaction_date.

Oracle Functions:
* NVL(expression, replacement_value)

    The NVL function is used to replace NULL values with a specified replacement value.   
    If the first argument is NULL, it returns the second argument. Otherwise, it returns the first argument.
* BITAND(x, y)				

    The BITAND function performs a bitwise AND operation between two integer values. 

### MySQL Implementation
```sql
SELECT DISTINCT transaction_date, 
SUM(CASE WHEN amount % 2=0 THEN 0 ELSE amount END) OVER( PARTITION BY transaction_date  ) AS odd_sum,
SUM(CASE WHEN amount % 2=0 THEN amount ELSE 0 END) OVER( PARTITION BY transaction_date  ) AS even_sum
FROM transactions;
```
### Oracle Implementation
```sql
SELECT TO_CHAR(transaction_date,'yyyy-MM-dd') AS transaction_date,
       NVL(SUM(CASE WHEN BITAND(amount,1)=1 THEN amount END),0) AS odd_sum,
       NVL(SUM(CASE WHEN BITAND(amount,1)=0 THEN amount END),0) AS even_sum
FROM transactions
GROUP BY transaction_date
ORDER BY transaction_date ASC;
```
# Uncategorized Problems
## Distribute Elements Into Two Arrays II
[Back to Top](#table-of-contents) 
### Overview
You are given a **1-indexed** array of integers `nums` of length `len`.

We define a function `greaterCount` such that `greaterCount(arr, val)` returns the number of elements in `arr` that are strictly greater than `val`.

You need to distribute all the elements of `nums` between two arrays `arr1` and `arr2` using `len` operations. In the first operation, append `nums[1]` to `arr1`. In the second operation, append `nums[2]` to `arr2`. Afterwards, in the i<sup>th</sup> operation:

* If `greaterCount(arr1, nums[i]) > greaterCount(arr2, nums[i])`, append `nums[i]` to `arr1`.
* If `greaterCount(arr1, nums[i]) < greaterCount(arr2, nums[i])`, append `nums[i]` to `arr2`.
* If `greaterCount(arr1, nums[i]) == greaterCount(arr2, nums[i])`, append `nums[i]` to the array with a lesser number of elements.
* If there is still a tie, append `nums[i]` to `arr1`.

The array result is formed by concatenating the arrays `arr1` and `arr2`. For example, if `arr1 == [1,2,3]` and `arr2 == [4,5,6]`, then `result = [1,2,3,4,5,6]`.

Return the integer array `result`.

**Example 1:**

> **Input:** nums = [2,1,3,3]  
> **Output:** [2,3,1,3]  
> **Explanation:** After the first 2 operations, arr1 = [2] and arr2 = [1].  
> In the 3rd operation, the number of elements greater than 3 is zero in both arrays. Also, the lengths are equal, hence, append nums[3] to arr1.  
> In the 4th operation, the number of elements greater than 3 is zero in both arrays. As the length of arr2 is lesser, hence, append nums[4] to arr2.  
> After 4 operations, arr1 = [2,3] and arr2 = [1,3].  
> Hence, the array result formed by concatenation is [2,3,1,3].  

**Example 2:**

> **Input:** nums = [5,14,3,1,2]  
> **Output:** [5,3,1,2,14]  
> **Explanation:** After the first 2 operations, arr1 = [5] and arr2 = [14].  
> In the 3rd operation, the number of elements greater than 3 is one in both arrays. Also, the lengths are equal, hence, append nums[3] to arr1.  
> In the 4th operation, the number of elements greater than 1 is greater in arr1 than arr2 (2 > 1). Hence, append nums[4] to arr1.  
> In the 5th operation, the number of elements greater than 2 is greater in arr1 than arr2 (2 > 1). Hence, append nums[5] to arr1.  
> After 5 operations, arr1 = [5,3,1,2] and arr2 = [14].  
> Hence, the array result formed by concatenation is [5,3,1,2,14].

**Example 3:**

> **Input:** nums = [3,3,3,3]  
> **Output:** [3,3,3,3]  
> **Explanation:** At the end of 4 operations, arr1 = [3,3] and arr2 = [3,3].  
> Hence, the array result formed by concatenation is [3,3,3,3].

**Constraints:**
* `3 <= n <= 10^5`
* `1 <= nums[i] <= 10^9`

### Analysis
Follow the problem description, the key point is to find the number of elements in array `nums` that are strictly greater than val.


A Binary Indexed Tree (BIT) is ideal for scenarios that require frequent updates to an array and efficient calculation of prefix sums or ranges, making it a suitable choice here.

To achieve this, define an array `tree` within a `BinaryIndexedTree` class and use bitwise operation to identify the indexes to update and compute the prefix sum.
* i += i & -i
    
    Isolate the least significant set bit (LSB) of `i` and add it to `i`.
    Example:
    ```text
    40 + (40 & -40) 
        = 0010 1000 + (0010 1000  & 1101 1000)
        = 0010 1000 + 0000 1000 
        = 48
    ```
* i &= i - 1

    The operation clears the rightmost set bit (1) in the binary representation of `i`, it is equivalent to `i -= i & -i`.

    Example:
    ```text
    11 & 10 
        = 1011 & 1010 
        = 1010 
        = 10
    48 & 47
        = 0011 0000 & 0010 1111
        = 0010 0000
        = 32
    11 & 10
        = 1011 & 1010
        = 1010
    ```
    
This `tree` corresponds to a sorted version of the array `nums`, called `sortedArr`, and stores prefix sums rather than actual values.

When inserting a new element at index `i` into the Binary Indexed Tree, increment the value at index `i` of array `tree` by `1`.  
This update allows the tree to maintain a count of elements before index `i` in the array `tree`, which represents the number of elements in `nums` that are less than `sortedArr[i]`.

#### Implementation
```java
class BinaryIndexedTree {
    private final int[] tree;

    public BinaryIndexedTree(int len) {
        tree = new int[len];
    }

    public void add(int i) {
        while (i < tree.length) {
            tree[i]++;
            i += i & -i;
        }
    }

    public int prefixSum(int i) {
        int res = 0;
        while (i > 0) {
            res += tree[i];
            i &= i - 1;
        }
        return res;
    }
}

class Solution {
    public int[] resultArray(int[] nums) {
        int[] sortedArr = nums.clone();
        Arrays.sort(sortedArr); 

        int len = nums.length;
        List<Integer> list1 = new ArrayList<>(len);
        List<Integer> list2 = new ArrayList<>();
        list1.add(nums[0]);
        list2.add(nums[1]);

        BinaryIndexedTree bit1 = new BinaryIndexedTree(len + 1);
        BinaryIndexedTree bit2 = new BinaryIndexedTree(len + 1);
        bit1.add(Arrays.binarySearch(sortedArr, nums[0]) + 1);
        bit2.add(Arrays.binarySearch(sortedArr, nums[1]) + 1);
        // Traverse array 'nums'
        for (int i = 2; i < nums.length; i++) {
            int cu = nums[i];
            // Search the index of value 'cu' in array 'sortedArr'.
            int sInd = Arrays.binarySearch(sortedArr, cu) + 1;
            int gc1 = list1.size() - bit1.prefixSum(sInd); 
            int gc2 = list2.size() - bit2.prefixSum(sInd); 
            if (gc1 > gc2 || gc1 == gc2 && list1.size() <= list2.size()) {
                list1.add(cu);
                bit1.add(sInd);
            } else {
                list2.add(cu);
                bit2.add(sInd);
            }
        }
        // Concatenate the two lists
        list1.addAll(list2);

        // Convert the list into a primitive array
        for (int i = 0; i < len; i++) {
            nums[i] = list1.get(i);
        }
        return nums;
    }
}
```
#### Time and Space Complexity
* Time Complexity: $ O(nlogn) $\
    * Traverse array 'nums'
        The `for` loop iterates over the array `nums`, taking $O(n)$ time.
        The `binarySearch` and `prefixSum` methods each contribute $O(logn)$ time complexity.

        Therefore, the overall time complexity for this part is $O(nlogn)$.
    * Convert the list into a primitive array

        Traversing the array `nums` takes $O(n)$ time.

    Thus, the overall time complexity is $O(nlogn)$.

* Space Complexity: $ O(n) $

    The array `sortedArr`, lists `list1` and `list2`, and binary indexed tree `bit1` and `bit2` each contribute $O(n)$ to the space complexity.  
    Therefore, the total space complexity is $O(n)$.

## Merge Sorted Array
[Back to Top](#table-of-contents)  
### Overview
You are given two integer arrays `nums1` and `nums2`, sorted in non-decreasing order, and two integers `m` and `n`, representing the number of elements in `nums1` and `nums2` respectively.

Merge `nums1` and `nums2` into a single array sorted in non-decreasing order.

The final sorted array should not be returned by the function, but instead be stored inside the array `nums1`. To accommodate this, `nums1` has a length of `m + n`, where the first `m` elements denote the elements that should be merged, and the last `n` elements are set to `0` and should be ignored. `nums2` has a length of `n`.

**Example 1:**
> **Input:** nums1 = [1,2,3,0,0,0], m = 3, nums2 = [2,5,6], n = 3  
> **Output:** [1,2,2,3,5,6]  
> **Explanation:** The arrays we are merging are [1,2,3] and [2,5,6].  
> The result of the merge is [1,2,2,3,5,6] with the underlined elements coming from nums1.

**Example 2:**
> **Input:** nums1 = [1], m = 1, nums2 = [], n = 0  
> **Output:** [1]  
> **Explanation:** The arrays we are merging are [1] and [].  
> The result of the merge is [1].

**Example 3:**
> **Input:** nums1 = [0], m = 0, nums2 = [1], n = 1  
> **Output:** [1]  
> **Explanation:** The arrays we are merging are [] and [1].  
> The result of the merge is [1].  
> Note that because m = 0, there are no elements in nums1. The 0 is only there to ensure the merge result can fit in nums1.
 

**Constraints:**
* `nums1.length == m + n`
* `nums2.length == n`
* `0 <= m, n <= 200`
* `1 <= m + n <= 200`
* `-10^9 <= nums1[i], nums2[j] <= 10^9`

### Analysis
Copy a new array `nums1Cp` from `nums1` as a comparing array, and merge elements from arrays 'nums1Cp' and 'nums2' into 'nums1':
Copy the array `nums1` into a new array `nums1Cp` for comparison, and merge elements from `nums1Cp` and `nums2` into `nums1` as follows:

* If both array `nums1Cp` and `nums2` have remaining elements and `nums1Cp[i] < nums2[k]`, insert the smaller value from `numsCp` into `nums1`.
* If `nums2` has no remaining elements, insert the current element from `nums1Cp` directly into `nums1`;
* If `nums1Cp` is empty or `nums2` contains the smaller value, insert the value from `nums2` into `nums1`.
### Implementation
```java
class Solution {
    public void merge(int[] nums1, int m, int[] nums2, int n) {
        int[] nums1Cp=Arrays.copyOf(nums1, m);
        // Merge elements from arrays 'nums1Cp' and 'nums2' into 'nums1'.
        for(int i=0, j=0, k=0; i<nums1.length; i++){ 
            if( j<m && k<n && nums1Cp[j]<nums2[k] || j<m  && k>=n  ){
                nums1[i]=nums1Cp[j];
                j++;
            }else {
                nums1[i]=nums2[k];
                k++;
            }
        }
    }
}
```
#### Time and Space Complexity
* Time Complexity: $ O(m + n) $\

    The `Arrays.copyOf` method takes $O(m)$ time and the `for` loop takes $O(n)$ time, 
    resulting in an overall time complexity of $O(m + n)$.

* Space Complexity: $ O(m) $

    The array `nums1Cp` occupies $O(m)$ space where `m` is the first `m` integers in array `nums1`.

## Max Difference You Can Get From Changing an Integer
[Back to Top](#table-of-contents)  
### Overview

You are given an integer `num`. You will apply the following steps exactly **two** times:
* Pick a digit `x` `(0 <= x <= 9)`.
* Pick another digit `y` `(0 <= y <= 9)`. The digit `y` can be equal to `x`.
* Replace all the occurrences of `x` in the decimal representation of `num` by `y`.
* The new integer **cannot** have any leading zeros, also the new integer **cannot** be 0.

Let `a` and `b` be the results of applying the operations to `num` the first and second times, respectively.

Return the max difference between `a` and `b`.

**Example 1:**
> **Input:** num = 555  
> **Output:** 888  
> **Explanation:**   
The first time pick x = 5 and y = 9 and store the new integer in a.  
The second time pick x = 5 and y = 1 and store the new integer in b.  
We have now a = 999 and b = 111 and max difference = 888

**Example 2:**
> **Input:** num = 9
> **Output:** 8
> **Explanation:** The first time pick x = 9 and y = 9 and store the new integer in a.  
The second time pick x = 9 and y = 1 and store the new integer in b.  
We have now a = 9 and b = 1 and max difference = 8

**Constraints:**

* `1 <= num <= 10^8`

### Analysis
To find the maximum difference between `a` and `b`, we need to determine the maximum value  (`max`) and minimum value (`min`), where `a` and `b` can be any of these values.

Consider the following cases:
* To obtain the maximum value, replace the first non-`9` digit with `9` starting from the highest posiiton.
* To obtain the minimum value, replace the first non-`0` digit with `0` starting from the highest position, except for the first digit, which cannot be `0`. If the first digit is not `1`, replace it with `1`.
### Implementation
```java
class Solution {
    public int maxDiff(int num) {
        String nStr=String.valueOf(num);
        int max=num;
        int min=num;
        for(int i=0; i< nStr.length(); i++){
            if(nStr.charAt(i)<'9' && max==num){
                max=Integer.parseInt(nStr.replace(nStr.charAt(i), '9'));
                if(min!=num)break;
            }
            if(i==0 && nStr.charAt(i)>'1' && min==num){
                min=Integer.parseInt(nStr.replace(nStr.charAt(i), '1'));
                if(max!=num)break;
            }else if(i>0 && nStr.charAt(i)>'0' && nStr.charAt(i)!=nStr.charAt(0) && min==num){
                min=Integer.parseInt(nStr.replace(nStr.charAt(i), '0'));
                if(max!=num)break;
            }
        }
        return Math.abs(max-min);
    }
}
```
#### Time and Space Complexity
* Time Complexity: $ O(n) $

    Since the `replace` method has a time complexity of $O(n)$ and is executed at most twice, while `charAt` and `Integer.parseInt` take $O(1)$ time, the overall time complexity of the `for` is $O(n)$.
* Space Complexity: $ O(1) $

## Egg Drop With 2 Eggs and N Floors
[Back to Top](#table-of-contents)  
### Overview
You are given two identical eggs and you have access to a building with `n` floors labeled from `1` to `n`.

You know that there exists a floor `f` where `0 <= f <= n` such that any egg dropped at a floor higher than `f` will break, and any egg dropped at or below floor `f` will not break.

In each move, you may take an unbroken egg and drop it from any floor `x` (where `1 <= x <= n`). If the egg breaks, you can no longer use it. However, if the egg does not break, you may reuse it in future moves.

Return the minimum number of moves that you need to determine with certainty what the value of `f` is.


**Example 1:**
> **Input:** n = 2  
> **Output:** 2  
> **Explanation:**  
 We can drop the first egg from floor 1 and the second egg from floor 2.  
If the first egg breaks, we know that f = 0.  
If the second egg breaks but the first egg didn't, we know that f = 1.  
Otherwise, if both eggs survive, we know that f = 2.

**Example 2:**
> **Input:** n = 100  
> **Output:** 14  
> **Explanation:** One optimal strategy is:  
> - Drop the 1st egg at floor 9. If it breaks, we know f is between 0 and 8.  
    Drop the 2nd egg starting from floor 1 and going up one at a time to find f within 8 more drops.  
    Total drops is 1 + 8 = 9.
> - If the 1st egg does not break, drop the 1st egg again at floor 22.  
    If it breaks, we know f is between 9 and 21. Drop the 2nd egg starting from floor 10 and going up one at a time to find f within 12 more drops.  
    Total drops is 2 + 12 = 14.
> - If the 1st egg does not break again, follow a similar process dropping the 1st egg from floors 34, 45, 55, 64, 72, 79, 85, 90, 94, 97, 99, and 100.  
    Regardless of the outcome, it takes at most 14 drops to determine f.

**Constraints:**
* `1 <= n <= 1000`

### Analysis
Dropping the egg from floor `1` to the top floor `n` is the simplest method, but with the second egg, we can narrow down the approximate range where the floor `f` is located.

* Case 1 

    Assuming there are `100` floors, If the egg is dropped from floor `1` with a gap of `4`, the drop sequence will be::
    ```text
    1, 4, 8, 12 ... 100.
    ```
    In the worst case, the first egg may require `25` drops, and the second egg `3` drops.
* Case 2
    
    If the egg is dropped with a gap of `10`, The first egg will be dropped `10` times and the second egg `9` times in the worst case, significantly reducing the total number of drops.
    ```text
    1, 10, 20, 30, ... 100.
    ```
    By analyzing the pattern of egg drops, we aim to minimize the worst-case number of drops.
    We can allocate drop chances to lower floors to ensure the worst-case number of drops remains consistent, regardless of when the first egg breaks.

    For example, If the first egg breaks at floor `10`, we drop the second egg from floor `1` to `9`, resulting in a maximum of `10` drops.  
    However, the wrost-case total drops would be `19`.
    We aim to keep the worst number of drops consistent across all first drop locations.

* Case 3

    If the drop gap decreases by `1` with each subsequent drop, the worst drop times will remain the same for each check floor.
    Therefore, we start with the highest possible floor `n` and gradually increase the gap by `1`, ensuring the worst drop times are consistent.

    If The remaining floors, which are fewer thant the last gap, result in fewer worst-case drops, they will still be treated as regular drops.

Assuming we need to drop the first egg `j` times, we get:

$$ 1+2+3+...+j = n $$
Based on the formula of geometric series:
$$ \sum_{j=1}^j = 1+2+3+...+j = \frac{j \times (j+1)}{2}   $$
we have:
$$ j^2+j = 2n $$

Based on the formula for the sum of a geometric series:
$$ ax^2+bx+c=0 $$
$$ x=\frac{-b \plusmn \sqrt{b^2-4ac}}{2a} $$
We have:
$$ j=\frac{-1 \plusmn \sqrt{1+8n}}{2} $$ 

Because we expect a positive number, the formula will be:

$$ j=\frac{-1 + \sqrt{1+8n}}{2} $$ 
This gives the smallest integer $j$, where the fractional part is treated as a full drop.

#### Implementation
```java
class Solution {
    public int twoEggDrop(int n) {
        double sqrt=Math.sqrt((double)(1+8*n));
        double j=(-1+sqrt)/2;
        return (int)Math.ceil(j);
    }
}
```
#### Time and Space Complexity
* Time Complexity: $ O(1) $
* Space Complexity: $ O(1) $

## Maximum Length of Subarray With Positive Product
[Back to Top](#table-of-contents)
### Overview
Given an array of integers `nums`, find the maximum length of a subarray where the product of all its elements is positive.

A subarray of an array is a consecutive sequence of zero or more values taken out of that array.

Return the maximum length of a subarray with positive product.

**Example 1:**
> **Input:** nums = [1,-2,-3,4]  
> **Output:** 4  
> **Explanation:** The array nums already has a positive product of 24.

**Example 2:**
> **Input:** nums = [0,1,-2,-3,-4]  
> **Output:** 3  
> **Explanation:** The longest subarray with positive product is [1,-2,-3] which has a product of 6.
Notice that we cannot include 0 in the subarray since that'll make the product 0 which is not positive.

**Example 3:**
> **Input:** nums = [-1,-2,-3,0,1]  
> **Output:** 2  
> **Explanation:** The longest subarray with positive product is [-1,-2] or [-2,-3].

**Constraints:**
* `1 <= nums.length <= 10^5`
* `-10^9 <= nums[i] <= 10^9`

### Analysis
Use two variables (`positiveLen` and `negativeLen`) to track the length of subarrays with positive and negative products:
* If `num[i]` is positive, extend the current subarray length by `1`.
    ```
    1, 2, 0, 36, -32, -10
       i 
    ```
* If `nums[i]` is `0`, begin a new sequence to determine the maximum subarray length.
    ```
    -39, -5, 0, 36, -32, -10
             i 
    ```
* If `nums[i]` is nagative, swap `positiveLen` and `negativeLen`.  
    Example: 
     * Step 1:   
        ```
        7, -10, -7, -34, 26, 2
        i 
        positveLen=1
        negativeLen=0
    * Step 2:   
        ```
        7, -10, -7, -34, 26, 2
             i 
        positveLen=0
        negativeLen=2
        ```
        Swap the lengths when encountering a negative number and reset `positiveLen` when encountering the first neagative number.
    * Step 3:   
        ```
        7, -10, -7, -34, 26, 2
                 i 
        positveLen=3
        negativeLen=1
        ```
        Swap the lengths when encountering a negative number.  
        Since `posiitonLen` was reset in the previous step, `negativeLen` will be recalculated duiring the swap process.  
        The first negative number is the key point when encountering a sequence like `-34, 26, 2` **at the end**.  
        In this case, we aim to remove the first negative number `-10` and its left part `7` to obtain the valid subarray `-7, -34, 26, 2`.  
    * Step 4:   
        ```
        7, -10, -7, -34, 26, 2
                      i 
        positveLen=2
        negativeLen=4
        ```
        Swap the lengths as before.
    * Step 5:   
        ```
        7, -10, -7, -34, 26, 2
                          i 
        positveLen=3
        negativeLen=5
        ```
    * Step 6:   
        ```
        7, -10, -7, -34, 26, 2
                             i 
        positveLen=4
        negativeLen=6
        ```
### Implementation
```java
class Solution {
    public int getMaxLen(int[] nums) {
        int len = nums.length;
        // Track the length of the subarray with a positive product
        int positiveLen = 0;
        // Track the length of the subarray with a negative product 
        int negativeLen = 0;
        int maxLen = positiveLen;
        for (int i = 0; i < len; i++) {
            if (nums[i] > 0) {
                positiveLen++;
                negativeLen = negativeLen > 0 ? negativeLen + 1 : 0;
            } else if (nums[i] < 0) {
                // Swap the lengths when encountering a negative number
                int temp = positiveLen;
                // Reset positiveLen when encountering the first negative number
                positiveLen = negativeLen > 0 ? negativeLen + 1 : 0;
                negativeLen = temp+1;
            } else {
                // Reset the lengths to zero when a zero is encountered.
                positiveLen = 0;
                negativeLen = 0;
            }
            maxLen = Math.max(maxLen, positiveLen);
        }
        return maxLen;
    }
}
```
#### Time and Space Complexity
* Time Complexity: $ O(n) $

    The `for` loop has time complexity $O(n)$, resulting in an overall time complexity of $O(n)$.

* Space Complexity: $ O(1) $

## Add Edges to Make Degrees of All Nodes Even
[Back to Top](#table-of-contents)
### Overview
There is an **undirected** graph consisting of `n` nodes numbered from `1` to `n`. 
You are given the integer `n` and `a` **2D** array `edges` where `edges[i] = [a_i, b_i]` indicates that there is an edge between nodes `a_i` and `b_i`. The graph can be disconnected.

You can add at most two additional edges (possibly none) to this graph so that there are no repeated edges and no self-loops.

Return `true` if it is possible to make the degree of each node in the graph even, otherwise return `false`.

The degree of a node is the number of edges connected to it.

**Example 1:**  
![aetmdoane1](assets/Algorithms/aetmdoane1.png)  
> **Input:** n = 5, edges = [[1,2],[2,3],[3,4],[4,2],[1,4],[2,5]]  
> **Output:** true  
> **Explanation:** The above diagram shows a valid way of adding an edge.
Every node in the resulting graph is connected to an even number of edges.

**Example 2:**  
![aetmdoane2](assets/Algorithms/aetmdoane2.png)  
> **Input:** n = 4, edges = [[1,2],[3,4]]  
> **Output:** true  
> **Explanation:** The above diagram shows a valid way of adding two edges.

**Example 3:**  
![aetmdoane3](assets/Algorithms/aetmdoane3.png)  
**Input:** n = 4, edges = [[1,2],[1,3],[1,4]]  
**Output:** false  
**Explanation:** It is not possible to obtain a valid graph with adding at most 2 edges.

**Constraints:**
* `3 <= n <= 10^5`
* `2 <= edges.length <= 10^5`
* `edges[i].length == 2`
* `1 <= a_i, b_i <= n`
* `a_i != b_i`
* There are no repeated edges.

### Analysis
To make all node degrees in the graph even by adding only two edges, the following conditions must be met:
* The number of nodes with odd degrees must be even and cannot exceed 4.
    * If there are 4 such nodes, two distinct pairs must exist that can be connected.
    * If there are 2 such nodes, they must either be unconnected, or a third node must exist that can connect to both.
#### Implementation
```java
class Solution {
    public boolean isPossible(int n, List<List<Integer>> edges) {
        Set[] connectedNodes = new Set[n + 1];
        Arrays.setAll(connectedNodes, item -> new HashSet<Integer>());

        // Record all nodes connected to each node
        for (List<Integer> edge : edges) {
            int node1 = edge.get(0), node2 = edge.get(1);
            connectedNodes[node1].add(node2);
            connectedNodes[node2].add(node1);
        }
        List<Integer> nodeWithOddEdges = new ArrayList<Integer>();

        // Identify all nodes with an odd degree
        for (int node = 1; node <= n; ++node){
            if(connectedNodes[node].size()%2>0){
                nodeWithOddEdges.add(node);
                if(nodeWithOddEdges.size()>4)return false;
            }
        }
        int len = nodeWithOddEdges.size();
        if (len == 0) return true;
        if (len == 2) {
            int node1 = nodeWithOddEdges.get(0), node2 = nodeWithOddEdges.get(1);
            // Verify if the two nodes are not connected
            if (!connectedNodes[node1].contains(node2)) return true;

            // Check if there is a third node that can conect to the two nodes
            for (int node = 1; node <= n; ++node){
                if (
                    node != node1 && node != node2 
                    && !connectedNodes[node].contains(node1) && !connectedNodes[node].contains(node2)){
                    return true;
                }
            }
            return false;
        }

        if (len == 4) {
            int a = nodeWithOddEdges.get(0), b = nodeWithOddEdges.get(1), c = nodeWithOddEdges.get(2), d = nodeWithOddEdges.get(3);
            return !connectedNodes[a].contains(b) && !connectedNodes[c].contains(d) ||
                    !connectedNodes[a].contains(c) && !connectedNodes[b].contains(d) ||
                    !connectedNodes[a].contains(d) && !connectedNodes[b].contains(c);
        }
    }
}
```
#### Time and Space Complexity
* Time Complexity: $ O(m+n) $  (`m` is the length of array `edges`)
    * `Arrays.setAll`   
    
        Method `Arrays.setAll` takes a time complexiyt of $O(n)$.
    * Record all nodes connected to each node

        The `for` loop has a time complexity of $O(m)$ where `m` is the length of `edges` array.
    * Identify all nodes with an odd degree
        
        The `for` loop takes a time complexiyt of $O(n)$.
    * Check if there is a third node that can conect to the two nodes
        
        The `for` loop takes a time complexiyt of $O(n)$.

    Therefore, the overall time complexity is $O(m+n)$.
* Space Complexity: $ O(m+n) $
    * The `connectedNodes` Set array takes $O(m+n)$ space in the worst case, where each node is connected to almost every other node.
    * The `nodeWithOddEdges` array has a size of at most `4`, so its space complexity is constant and can be omitted.

    Thus, the total space complexity is $O(m+n)$.

## Amount of Time for Binary Tree to Be Infected
[Back to Top](#table-of-contents)
### Overview
You are given the `root` of a binary tree with unique values, and an integer `start`. 
At minute `0`, an infection starts from the node with value `start`.

Each minute, a node becomes infected if:
* The node is currently uninfected.
* The node is adjacent to an infected node.
Return the number of minutes needed for the entire tree to be infected.

**Example 1:**

![aoftfbttb1](assets/Algorithms/aoftfbttb1.png)
> **Input:** root = [1,5,3,null,4,10,6,9,2], start = 3  
> **Output:** 4  
> **Explanation:** The following nodes are infected during:  
    - Minute 0: Node 3  
    - Minute 1: Nodes 1, 10 and 6  
    - Minute 2: Node 5  
    - Minute 3: Node 4  
    - Minute 4: Nodes 9 and 2  
    It takes 4 minutes for the whole tree to be infected so we return 4.

**Example 2:**

![aoftfbttb2](assets/Algorithms/aoftfbttb2.png)
> **Input:** root = [1], start = 1  
> **Output:** 0  
> **Explanation:** At minute 0, the only node in the tree is infected so we return 0.

**Constraints:**
* The number of nodes in the tree is in the range `[1, 10^5]`.
* `1 <= Node.val <= 10^5`
* Each node has a unique value.
* A node with a value of `start` exists in the tree.

### Analysis
#### Implementation
```java
class Solution {
    private Class Vo{
        public boolean hasStartNode;
        public int pathLen;
        public Vo(boolean hasStartNode, int pathLen){
            this.hasStartNode=hasStartNode;
            this.pathLen=pathLen;
        }
    }

    private int longestPath=0;

    public int amountOfTime(TreeNode root, int start) {
        return dfs(root, start, new Vo(false,0));
    }

    public Vo dfs(TreeNode root, int start, Vo vo) {
        if(root==null)return vo;
        if(root.val==start){
            vo.hasStartNode=true;
        }
        Vo vo1=dfs(root.left, start, new Vo(vo.hasStartNode, ++vo.pathLen));
        Vo vo2=dfs(root.right, start, new Vo(vo.hasStartNode, ++vo.pathLen));

        // Reuse the recursion result
        if(vo.hasStartNode){
            longestPath=Math.max(longestPath, vo2.pathLen + ) 
        }
        if(vo1.hasStartNode){
        }
        if(vo2.hasStartNode){
        }
        // TODO Merge the pathLen from left and right nodes.
    }
}
```