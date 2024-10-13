# https://leetcode.cn/problems/friday-purchases-ii/description/
# ----------------------------------------------------- Conditions
# Primary key:  (user_id, purchase_date, amount_spend)
# each Friday of every week in November 2023. 5 10 15 20 25 30
# total spending by users
# [ER] List all results for those Fridays regardless of whether the record exits.
# [IM] Functions: DATE_ADD,  DAYOFWEEK, CEIL, COALESCE
# [IM] Operators: WITH RECURSIVE ... AS, INTERVAL ... DAY
# [IM} Because the GROUP BY clause is processed before the SELECT clause, the alias defined in the SELECT clause isn't yet available for grouping.
# ---------------------------------------------------- Updated Solution
# Define a common table expression.
# DAYOFWEEK: Sunday is 1 and Saturday is 7.
WITH RECURSIVE cte AS(
    SELECT DATE_ADD('2023-11-01', INTERVAL (6 - DAYOFWEEK('2023-11-01')+7)%7 DAY) AS friday
    UNION ALL
    SELECT DATE_ADD(friday, INTERVAL 7 DAY) AS friday FROM cte
    WHERE  MONTH(DATE_ADD(friday, INTERVAL 7 DAY))=11
)
#     | friday     |
#     | ---------- |
#     | 2023-11-03 |
#     | 2023-11-10 |
#     | 2023-11-17 |
#     | 2023-11-24 |
SELECT CEIL(DAY(cte.friday)/7) AS week_of_month, cte.friday AS purchase_date, IFNULL(SUM(amount_spend), 0) AS total_amount
FROM cte
LEFT JOIN Purchases p
ON cte.friday = p.purchase_date
#GROUP BY purchase_date;
GROUP BY cte.friday;
# ---------------------------------------------------- Previous Solution
SELECT (DAY(purchase_date)-3)/7 +1 AS week_of_month, purchase_date, SUM(amount_spend) AS total_amount
FROM Purchases
WHERE MONTH(purchase_date)=11 AND YEAR(purchase_date)=2023 AND DAY(purchase_date) in (3,10,17,24)
GROUP BY purchase_date