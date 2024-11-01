# Write your MySQL query statement below
# https://leetcode.cn/problems/movie-rating/description/
# ---------------------------------------------------- Updated Solution
(SELECT  u.name AS results
 FROM Users u, MovieRating mr WHERE u.user_id=mr.user_id
 GROUP BY u.user_id
 #[IM] Use LIMIT to filter out a specified number of records.
 ORDER BY COUNT(mr.rating) DESC, u.name ASC  LIMIT 1)
UNION ALL
(SELECT m.title AS results
 FROM Movies  m
          LEFT JOIN MovieRating mr
                    #[IM] Function: YEAR, MONTH
                    ON m.movie_id=mr.movie_id AND YEAR(mr.created_at) = 2020 AND MONTH(mr.created_at) = 2
 GROUP BY mr.movie_id
 ORDER BY AVG(mr.rating) DESC, title ASC LIMIT 1)
