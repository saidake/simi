# Write your MySQL query statement below
# https://leetcode.cn/problems/movie-rating/description/
# ---------------------------------------------------- Previous Solution
SELECT r1.name, r2.title
FROM
 (SELECT u.user_id, u.name, COUNT(mr) FROM USERS u, MovieRating mr WHERE u.user_id=mr.user_id GROUP BY u.user_id ORDER BY COUNT(mr) DESC) r1,
 (SELECT m.movie_id, m.title, AVG(mr) FROM MOVIES m, MovieRating mr WHERE m.movie_id=mr.user_id GROUP BY u.user_id ORDER BY AVG(mr) DESC) r2
WHERE
# ...