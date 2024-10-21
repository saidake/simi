package com.simi.algorithm.math;

/**
 * https://leetcode.cn/problems/account-balance-after-rounded-purchase/
 * Account Balance After Rounded Purchase
 * Constraints:
 *     0 <= purchaseAmount <= 100
 */
public class AccountBalanceAfterRoundedPurchase {
    /**
     * Passed solution for "AccountBalanceAfterRoundedPurchase".
     *
     * @param purchaseAmount    Initial purchase amount
     * @return  final balance
     */
    public int accountBalanceAfterPurchase(int purchaseAmount) {
        int remainder=purchaseAmount%10;
        int roundPurchaseAmount=remainder>=5?purchaseAmount+10-remainder:purchaseAmount-remainder;
        return 100-roundPurchaseAmount;
    }
}
