package com.simi.common.util.data;

import java.time.*;
import java.time.format.DateTimeFormatter;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;

/**
 * Utility class for handling date and time operations with various formats.
 * Provides methods to convert between different date-time types, compare times,
 * and perform date arithmetic such as adding minutes, days, months, and years.
 */
public final class LocalDateTimeUtil {

    public static final DateTimeFormatter timeFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").withZone(ZoneId.systemDefault());
    public static final DateTimeFormatter timeStrFormat = DateTimeFormatter.ofPattern("yyyyMMddHHmmss").withZone(ZoneId.systemDefault());
    public static final DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd").withZone(ZoneId.systemDefault());
    public static final DateTimeFormatter date8Format = DateTimeFormatter.ofPattern("yyyyMMdd").withZone(ZoneId.systemDefault());
    public static final DateTimeFormatter dateWeekFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd EE").withZone(ZoneId.systemDefault()).withLocale(Locale.CHINA);

    /**
     * Get the current date in the "yyyy-MM-dd" format.
     *
     * @return The current date in "yyyy-MM-dd" format.
     */
    public static String getDateTimeMD() {
        return dateFormat.format(Instant.now());
    }

    /**
     * Get the current timestamp in "yyyyMMddHHmmss" format.
     *
     * @return The current timestamp as a string in "yyyyMMddHHmmss" format.
     */
    public static String getDateTimesTamp() {
        return timeStrFormat.format(Instant.now());
    }

    /**
     * Get the date (in "yyyy-MM-dd") corresponding to a given timestamp in milliseconds.
     *
     * @param millis The timestamp in milliseconds.
     * @return The formatted date in "yyyy-MM-dd".
     */
    public static String getSecondTime(long millis) {
        return dateFormat.format(Instant.ofEpochMilli(millis));
    }

    /**
     * Get the current date in "yyyyMMdd" format.
     *
     * @return The current date in "yyyyMMdd" format.
     */
    public static String getDate8Time() {
        return date8Format.format(Instant.now());
    }

    /**
     * Get the current date and time in "yyyy-MM-dd HH:mm:ss" format.
     *
     * @return The current date and time in "yyyy-MM-dd HH:mm:ss" format.
     */
    public static String getDateTime() {
        return timeFormat.format(Instant.now());
    }

    private LocalDateTimeUtil() {
    }

    /**
     * Convert a Date object to a "yyyyMMdd" formatted string.
     *
     * @param date The Date object to convert.
     * @return The formatted date as a string in "yyyyMMdd" format.
     */
    public static String getDate8TimeforDate(Date date) {
        LocalDateTime localDateTime = dateToLocalDateTime(date);
        return localDateTime.format(date8Format);
    }

    /**
     * Compare the given time with the current time.
     *
     * @param time The time to compare with the current time.
     * @return True if the given time is before the current time, false if after.
     */
    public static boolean timeIsOver(Date time) {
        boolean isOver = true;
        Calendar c1 = Calendar.getInstance();
        Calendar c2 = Calendar.getInstance();
        c1.setTime(new Date());
        c2.setTime(time);
        int result = c1.compareTo(c2);
        if (result <= 0) {
            isOver = false;
        }
        return isOver;
    }

    /**
     * Compare two given times.
     *
     * @param time1 The first time.
     * @param time2 The second time.
     * @return True if the first time is before the second, false if after.
     */
    public static boolean timeIsOver(Date time1, Date time2) {
        boolean isOver = true;
        Calendar c1 = Calendar.getInstance();
        Calendar c2 = Calendar.getInstance();
        c1.setTime(time1);
        c2.setTime(time2);
        int result = c1.compareTo(c2);
        if (result <= 0) {
            isOver = false;
        }
        return isOver;
    }

    /**
     * Compare a given time string (in "yyyy-MM-dd HH:mm:ss" format) with the current time.
     *
     * @param text The time string to compare (example: "2017-01-01 01:01:01").
     * @return True if the given time is after the current time, false if before.
     */
    public static boolean TimeIsOver(String text) {
        LocalDateTime localDateTime = LocalDateTime.parse(text, timeFormat);
        LocalDateTime nowTime = LocalDateTime.now();
        return localDateTime.isAfter(nowTime);
    }

    /**
     * Add a specified number of minutes to the current time and return the result in a formatted string.
     *
     * @param mins The number of minutes to add.
     * @param formatter The formatter to apply to the resulting time.
     * @return The new time as a string.
     */
    public static String AddMins(int mins, DateTimeFormatter formatter) {
        LocalDateTime nowTime = LocalDateTime.now();
        LocalDateTime localDateTime = nowTime.plusMinutes(mins);
        return localDateTime.format(formatter);
    }

    /**
     * Add a specified number of days to the current time and return the result in a formatted string.
     *
     * @param days The number of days to add.
     * @param formatter The formatter to apply to the resulting date.
     * @return The new date as a string.
     */
    public static String AddDays(int days, DateTimeFormatter formatter) {
        LocalDateTime nowTime = LocalDateTime.now();
        LocalDateTime localDateTime = nowTime.plusDays(days);
        return localDateTime.format(formatter);
    }

    /**
     * Add a specified number of days to a given date string and return the result in a formatted string.
     *
     * @param days The number of days to add.
     * @param text The date string (example: "2023-01-01").
     * @param formatter The formatter to apply to the resulting date.
     * @return The new date as a string.
     */
    public static String addDaysinS(int days, String text, DateTimeFormatter formatter) {
        LocalDate time = LocalDate.parse(text, formatter);
        LocalDate localDate = time.plusDays(days);
        return localDate.format(formatter);
    }

    /**
     * Add a specified number of months to the current time and return the result in a formatted string.
     *
     * @param months The number of months to add.
     * @param formatter The formatter to apply to the resulting date.
     * @return The new date as a string.
     */
    public static String AddMonths(int months, DateTimeFormatter formatter) {
        LocalDateTime nowTime = LocalDateTime.now();
        LocalDateTime localDateTime = nowTime.plusMonths(months);
        return localDateTime.format(formatter);
    }

    /**
     * Add a specified number of years to the current time and return the result in a formatted string.
     *
     * @param years The number of years to add.
     * @param formatter The formatter to apply to the resulting date.
     * @return The new date as a string.
     */
    public static String AddYears(int years, DateTimeFormatter formatter) {
        LocalDateTime nowTime = LocalDateTime.now();
        LocalDateTime localDateTime = nowTime.plusYears(years);
        return localDateTime.format(formatter);
    }

    /**
     * Convert a date string (in "yyyy-MM-dd HH:mm:ss" format) to a Date object.
     *
     * @param text The date string (example: "2017-01-01 01:01:01").
     * @return The corresponding Date object.
     */
    public static Date StringToDate(String text) {
        LocalDateTime localDateTime = LocalDateTime.parse(text, timeFormat);
        long mils1 = localDateTime.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
        return new Date(mils1);
    }

    /**
     * Convert a date string (in "yyyy-MM-dd" format) to a Date object.
     *
     * @param text The date string (example: "2017-01-01").
     * @return The corresponding Date object.
     */
    public static Date StringToDateForLocalDate(String text) {
        Instant instant;
        try {
            LocalDate localDate = LocalDate.parse(text);
            instant = localDate.atStartOfDay().atZone(ZoneId.systemDefault()).toInstant();
        } catch (java.time.format.DateTimeParseException e) {
            throw new RuntimeException("Invalid date format.");
        }
        return Date.from(instant);
    }

    /**
     * Convert a Date object to a string in "yyyy-MM-dd HH:mm:ss" format.
     *
     * @param date The Date object to convert.
     * @return The corresponding date string.
     */
    public static String DateToStr(Date date) {
        Instant instant = date.toInstant();
        return timeFormat.format(instant);
    }

    /**
     * Convert a Date object to a LocalDateTime object.
     *
     * @param date The Date object to convert.
     * @return The corresponding LocalDateTime object.
     */
    public static LocalDateTime dateToLocalDateTime(Date date) {
        return LocalDateTime.ofInstant(date.toInstant(), ZoneId.systemDefault());
    }

    /**
     * Convert a LocalDateTime object to a Date object.
     *
     * @param localDateTime The LocalDateTime object to convert.
     * @return The corresponding Date object.
     */
    public static Date localDateTimeToDate(LocalDateTime localDateTime) {
        return Date.from(localDateTime.atZone(ZoneId.systemDefault()).toInstant());
    }
}
