/*
 https://leetcode.cn/problems/find-the-start-and-end-number-of-continuous-ranges/description/
 Logs
+---------------+---------+
| Column Name   | Type    |
+---------------+---------+
| log_id        | int     |
+---------------+---------+
 */
# [IM] "PARTITION BY" Usage
# [ER] Every derived table must have its own alias
# ---------------------------------------------------- Updated Solution
SELECT MIN(log_id) start_id, MAX(log_id) end_id
FROM (SELECT log_id, log_id - RANK() OVER( ORDER BY log_id) AS rn FROM `Logs`) tmp
GROUP BY tmp.rn;

