# https://leetcode.cn/problems/the-number-of-users-that-are-eligible-for-discount/description/
# (user_id, time_stamp) is the primary key
# if they had a purchase in the inclusive interval of time [startDate, endDate] with at least minAmount amount.
# ---------------------------------------------------- Passed Solution
CREATE FUNCTION getUserIDs(startDate DATE, endDate DATE, minAmount INT) RETURNS INT
BEGIN
        DECLARE userCount INT;
        SELECT COUNT(DISTINCT user_id) INTO userCount
        FROM Purchases
        WHERE time_stamp BETWEEN startDate AND endDate AND amount> minAmount;
        RETURN COALESCE(userCount, 0);
END