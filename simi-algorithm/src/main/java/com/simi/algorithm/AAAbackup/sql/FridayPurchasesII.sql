# https://leetcode.cn/problems/friday-purchases-ii/description/
# ----------------------------------------------------- Conditions
# Primary key:  (user_id, purchase_date, amount_spend)
# each Friday of every week in November 2023. 5 10 15 20 25 30
# total spending by users
# [ER] List all results for those Fridays regardless of whether the record exits.
# [IM] Functions: DATE_ADD,  DAYOFWEEK, CEIL, COALESCE
# [IM] Operators: WITH RECURSIVE ... AS, INTERVAL ... DAY
# [IM} Because the GROUP BY clause is processed before the SELECT clause, the alias defined in the SELECT clause isn't yet available for grouping.
# ---------------------------------------------------- Previous Solution
SELECT (DAY(purchase_date)-3)/7 +1 AS week_of_month, purchase_date, SUM(amount_spend) AS total_amount
FROM Purchases
WHERE MONTH(purchase_date)=11 AND YEAR(purchase_date)=2023 AND DAY(purchase_date) in (3,10,17,24)
GROUP BY purchase_date