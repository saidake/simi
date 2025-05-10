package com.simi.common.util.data;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

@Slf4j
public class DateUtil {

    public static final Long HOUR = 60 * 60 * 1000L;
    public static final Long DAY = 24 * 60 * 60 * 1000L;
    public static final String yyyyMMddHHmmss = "yyyy-MM-dd HH:mm:ss";
    public static final String yyyy = "yyyy";
    public static final String yyyyMMdd = "yyyy-MM-dd";
    public static final String DateHourPattern = "yyyy-MM-dd HH";
    public static final String ORDER_DATE_TIME = "yyyyMMddHHmmss";
    public static final String YEAR_MONTH_DAY = "yyyyMMdd";

    /**
     * Format date to "yyyy-MM-dd" pattern
     *
     * @param date the date
     * @return formatted date as String
     */
    public static String format(Date date) {
        return format(date, yyyyMMddHHmmss);
    }

    /**
     * Format date to the provided pattern
     *
     * @param date    the date
     * @param pattern the format pattern (e.g., DateUtil.DATE_TIME_PATTERN)
     * @return formatted date as String
     */
    public static String format(Date date, String pattern) {
        if (date != null) {
            SimpleDateFormat df = new SimpleDateFormat(pattern);
            return df.format(date);
        }
        return null;
    }

    /**
     * Convert string to date based on the given pattern
     *
     * @param strDate the date string
     * @param pattern the pattern (e.g., DateUtil.DATE_TIME_PATTERN)
     * @return the parsed date
     */
    public static Date stringToDate(String strDate, String pattern) {
        if (StringUtils.isBlank(strDate)) {
            return null;
        }

        DateTimeFormatter fmt = DateTimeFormat.forPattern(pattern);
        return fmt.parseLocalDateTime(strDate).toDate();
    }

    /**
     * Convert long timestamp to formatted date string
     *
     * @param strDate the timestamp
     * @param pattern the pattern (e.g., DateUtil.DATE_TIME_PATTERN)
     * @return the formatted date string
     */
    public static String longToDate(Long strDate, String pattern) {
        SimpleDateFormat df = new SimpleDateFormat(pattern);
        return df.format(new Date(strDate));
    }

    /**
     * Get the start and end date of a week
     *
     * @param week the week offset (e.g., 0 for this week, -1 for last week)
     * @return an array containing the start and end dates of the week
     */
    public static Date[] getWeekStartAndEnd(int week) {
        DateTime dateTime = new DateTime();
        org.joda.time.LocalDate date = new org.joda.time.LocalDate(dateTime.plusWeeks(week));

        date = date.dayOfWeek().withMinimumValue();
        Date beginDate = date.toDate();
        Date endDate = date.plusDays(6).toDate();
        return new Date[]{beginDate, endDate};
    }

    /**
     * Add or subtract seconds from the given date
     *
     * @param date    the date
     * @param seconds the number of seconds to add (negative to subtract)
     * @return the modified date
     */
    public static Date addDateSeconds(Date date, int seconds) {
        DateTime dateTime = new DateTime(date);
        return dateTime.plusSeconds(seconds).toDate();
    }

    /**
     * Add or subtract minutes from the given date
     *
     * @param date    the date
     * @param minutes the number of minutes to add (negative to subtract)
     * @return the modified date
     */
    public static Date addDateMinutes(Date date, int minutes) {
        DateTime dateTime = new DateTime(date);
        return dateTime.plusMinutes(minutes).toDate();
    }

    /**
     * Add or subtract hours from the given date
     *
     * @param date  the date
     * @param hours the number of hours to add (negative to subtract)
     * @return the modified date
     */
    public static Date addDateHours(Date date, int hours) {
        DateTime dateTime = new DateTime(date);
        return dateTime.plusHours(hours).toDate();
    }

    /**
     * Add or subtract days from the given date
     *
     * @param date the date
     * @param days the number of days to add (negative to subtract)
     * @return the modified date
     */
    public static Date addDateDays(Date date, int days) {
        DateTime dateTime = new DateTime(date);
        return dateTime.plusDays(days).toDate();
    }

    /**
     * Add or subtract weeks from the given date
     *
     * @param date  the date
     * @param weeks the number of weeks to add (negative to subtract)
     * @return the modified date
     */
    public static Date addDateWeeks(Date date, int weeks) {
        DateTime dateTime = new DateTime(date);
        return dateTime.plusWeeks(weeks).toDate();
    }

    /**
     * Add or subtract months from the given date
     *
     * @param date   the date
     * @param months the number of months to add (negative to subtract)
     * @return the modified date
     */
    public static Date addDateMonths(Date date, int months) {
        DateTime dateTime = new DateTime(date);
        return dateTime.plusMonths(months).toDate();
    }

    /**
     * Add or subtract years from the given date
     *
     * @param date  the date
     * @param years the number of years to add (negative to subtract)
     * @return the modified date
     */
    public static Date addDateYears(Date date, int years) {
        DateTime dateTime = new DateTime(date);
        return dateTime.plusYears(years).toDate();
    }

    /**
     * Calculate a time based on a base time, number, and unit
     *
     * @param baseTime the base time
     * @param num      the number to add (negative to subtract)
     * @param unit     the unit in milliseconds
     * @return the calculated time
     */
    public static Long timePlus(Long baseTime, int num, Long unit) {
        return baseTime + num * unit;
    }

    /**
     * Calculate a date based on a base time, number, and unit
     *
     * @param baseTime the base time
     * @param num      the number to add (negative to subtract)
     * @param unit     the unit in milliseconds
     * @return the calculated date
     */
    public static Date timePlus(Date baseTime, int num, Long unit) {
        return new Date(baseTime.getTime() + num * unit);
    }

    /**
     * Subtract time from a base time
     *
     * @param baseTime the base time
     * @param num      the number to subtract
     * @param unit     the unit in milliseconds
     * @return the calculated time
     */
    public static Long timeMinus(Long baseTime, int num, Long unit) {
        return baseTime - num * unit;
    }

    /**
     * Subtract time from a base time
     *
     * @param baseTime the base time
     * @param num      the number to subtract
     * @param unit     the unit in milliseconds
     * @return the calculated date
     */
    public static Date timeMinus(Date baseTime, int num, Long unit) {
        return new Date(baseTime.getTime() - num * unit);
    }

    /**
     * Convert date to string based on the given format
     *
     * @param date   the date
     * @param format the format
     * @return the formatted string
     */
    public static String date2String(Date date, String format) {
        if (date == null) {
            return null;
        }
        SimpleDateFormat sdf = new SimpleDateFormat(format);
        return sdf.format(date);
    }

    /**
     * Convert string to date based on the given format
     *
     * @param dateString the string
     * @param format     the format
     * @return the parsed date
     */
    public static Date string2Date(String dateString, String format) {
        if (dateString == null) {
            return null;
        }
        SimpleDateFormat sdf = new SimpleDateFormat(format);
        Date parse = null;
        try {
            parse = sdf.parse(dateString);
        } catch (ParseException e) {
            log.error("Exception occurred: ", e);
        }
        return parse;
    }
}
