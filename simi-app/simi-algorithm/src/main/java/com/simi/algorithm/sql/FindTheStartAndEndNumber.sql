/*
 https://leetcode.cn/problems/find-the-start-and-end-number-of-continuous-ranges/description/
 Logs
+---------------+---------+
| Column Name   | Type    |
+---------------+---------+
| log_id        | int     |
+---------------+---------+
 */
SELECT log_id AS end_id
FROM `Logs` l1, `Logs` l2
WHERE NOT EXISTS(SELECT 1 FROM `Logs` WHERE l1.log_id+1=l2.log_id)




