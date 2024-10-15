# https://leetcode.cn/problems/the-number-of-users-that-are-eligible-for-discount/description/
# (user_id, time_stamp) is the primary key
# if they had a purchase in the inclusive interval of time [startDate, endDate] with at least minAmount amount.
# ---------------------------------------------------- Previous Solution
CREATE FUNCTION getUserIDs(startDate DATE, endDate DATE, minAmount INT) RETURNS INT
BEGIN
    RETURN (
        SELECT COUNT(user_id) AS user_cnt
        FROM Purchases
        WHERE time_stamp BETWEEN startDate AND endDate
        GROUP BY user_id
        HAVING SUM(amount) > minAmount
    );
END
# ---------------------------------------------------- Passed Solution
CREATE FUNCTION getUserIDs(startDate DATE, endDate DATE, minAmount INT) RETURNS INT
BEGIN
        DECLARE userCount INT;
        SELECT COUNT(DISTINCT user_id) INTO userCount
        FROM Purchases
        WHERE time_stamp BETWEEN startDate AND endDate AND amount> minAmount;
        RETURN COALESCE(userCount, 0);
END