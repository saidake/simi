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

# Modified Solution
SELECT MIN(log_id) start_id, MAX(log_id) end_id
FROM (SELECT log_id, log_id - RANK() OVER( ORDER BY log_id) AS rn FROM `Logs`) tmp
GROUP BY tmp.rn




# Previous Solution
# SELECT log_id AS end_id
# FROM `Logs` l1, `Logs` l2
# WHERE NOT EXISTS(SELECT 1 FROM `Logs` WHERE l1.log_id+1=l2.log_id)




