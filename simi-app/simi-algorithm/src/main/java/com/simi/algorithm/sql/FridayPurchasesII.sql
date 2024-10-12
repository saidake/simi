# https://leetcode.cn/problems/friday-purchases-ii/description/
# ----------------------------------------------------- Conditions
# Primary key:  (user_id, purchase_date, amount_spend)
# each Friday of every week in November 2023. 5 10 15 20 25 30
# total spending by users
# ---------------------------------------------------- Updated Solution

# ---------------------------------------------------- Previous Solution
SELECT (DAY(purchase_date)-3)/7 +1 AS week_of_month, purchase_date, SUM(amount_spend) AS total_amount
FROM Purchases
WHERE MONTH(purchase_date)=11 AND YEAR(purchase_date)=2023 AND DAY(purchase_date) in (3,10,17,24)
GROUP BY purchase_date