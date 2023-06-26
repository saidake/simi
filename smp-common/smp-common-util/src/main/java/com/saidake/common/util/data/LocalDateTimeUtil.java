package com.saidake.common.util.data;


import java.time.*;
import java.time.format.DateTimeFormatter;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;

/**
 * @author duyj
 */
public final class LocalDateTimeUtil {

    public static final DateTimeFormatter timeFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss").withZone(ZoneId.systemDefault());
    public static final DateTimeFormatter timeStrFormat = DateTimeFormatter.ofPattern("yyyyMMddHHmmss").withZone(ZoneId.systemDefault());
    public static final DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd").withZone(ZoneId.systemDefault());
    public static final DateTimeFormatter date8Format = DateTimeFormatter.ofPattern("yyyyMMdd").withZone(ZoneId.systemDefault());
    public static final DateTimeFormatter dateWeekFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd EE").withZone(ZoneId.systemDefault()).withLocale(Locale.CHINA);

    public static String getDateTimeMD() {
        return dateFormat.format(Instant.now());
    }
    public static String getDateTimesTamp() {
        return timeStrFormat.format(Instant.now());
    }
    public static String getSecondTime(long millis) {
        return dateFormat.format(Instant.ofEpochMilli(millis));
    }
    public static String getDate8Time() {
        return date8Format.format(Instant.now());
    }

    public static String getDateTime() {
        return timeFormat.format(Instant.now());
    }

    private LocalDateTimeUtil() {
    }

    public static String getDate8TimeforDate(Date date) {
        LocalDateTime localDateTime = dateToLocalDateTime(date);
        return localDateTime.format(date8Format);
    }

    /**
     * 需要判断的时间（yyyy-MM-dd HH:mm）
     *
     * @return true 小于当前时间；false 大于当前时间；
     * @author Fan
     */
    public static boolean timeIsOver(Date time) {
        boolean IsOver = true;
        Calendar c1 = Calendar.getInstance();
        Calendar c2 = Calendar.getInstance();
        c1.setTime(new Date());
        c2.setTime(time);
        int result = c1.compareTo(c2);
        if (result <= 0) {
            IsOver = false;
        }
        return IsOver;
    }


    public static boolean timeIsOver(Date time1,Date time2) {
        boolean IsOver = true;
        Calendar c1 = Calendar.getInstance();
        Calendar c2 = Calendar.getInstance();
        c1.setTime(time1);
        c2.setTime(time2);
        int result = c1.compareTo(c2);
        if (result <= 0) {
            IsOver = false;
        }
        return IsOver;
    }
    /**
     * 需要判断的时间（yyyy-MM-dd HH:mm:ss）
     *
     * @param text 例：2017-01-01 01:01:01
     * @return true 大于当前时间；false 小于当前时间；
     * @author baiban
     */
    public static boolean TimeIsOver(String text) {
        LocalDateTime localDateTime = LocalDateTime.parse(text, timeFormat);
        LocalDateTime nowTime = LocalDateTime.now();
        return localDateTime.isAfter(nowTime);
    }

    /**
     * 增加多少分钟
     *
     * @param mins 例：1,2,3,4,5,6,7
     * @return 时间字符串
     * @author baiban
     */
    public static String AddMins(int mins, DateTimeFormatter formatter) {
        LocalDateTime nowTime = LocalDateTime.now();
        LocalDateTime localDateTime = nowTime.plusMinutes(mins);
        String timeStr = localDateTime.format(formatter);
        return timeStr;
    }

    /**
     * 增加多少天
     *
     * @param days 例：1,2,3,4,5,6,7
     * @return 时间字符串
     * @author baiban
     */
    public static String AddDays(int days, DateTimeFormatter formatter) {
        LocalDateTime nowTime = LocalDateTime.now();
        LocalDateTime localDateTime = nowTime.plusDays(days);
        String timeStr = localDateTime.format(formatter);
        return timeStr;
    }

    public static String addDaysinS(int days,String text, DateTimeFormatter formatter) {
        LocalDate time = LocalDate.parse(text, formatter);
        LocalDate localDate = time.plusDays(days);
        String timeStr = localDate.format(formatter);
        return timeStr;
    }

    /**
     * 增加多少月
     *
     * @param months 例：1,2,3,4,5,6,7
     * @return 时间字符串
     * @author baiban
     */
    public static String AddMonths(int months, DateTimeFormatter formatter) {
        LocalDateTime nowTime = LocalDateTime.now();
        LocalDateTime localDateTime = nowTime.plusMonths(months);
        String timeStr = localDateTime.format(formatter);
        return timeStr;
    }

    /**
     * 增加多少年
     *
     * @param years 例：1,2,3,4,5,6,7
     * @return 时间字符串
     * @author baiban
     */
    public static String AddYears(int years, DateTimeFormatter formatter) {
        LocalDateTime nowTime = LocalDateTime.now();
        LocalDateTime localDateTime = nowTime.plusYears(years);
        String timeStr = localDateTime.format(formatter);
        return timeStr;
    }

    /**
     * String 转 Date
     *
     * @param text 例：2017-01-01 01:01:01
     * @return 日期
     * @author baiban
     */
    public static Date StringToDate(String text) {
        LocalDateTime localDateTime = LocalDateTime.parse(text, timeFormat);
        long mils1 = localDateTime.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
        return new Date(mils1);
    }

    /**
     * String 转 Date
     *
     * @param text 例：2017-01-01
     * @return 日期
     * @author baiban
     */
    public static Date StringToDateForLocalDate(String text) {
        Instant instant;
        try {
            LocalDate localDate = LocalDate.parse(text);
            instant = localDate.atStartOfDay().atZone(ZoneId.systemDefault()).toInstant();
        }catch (java.time.format.DateTimeParseException e){
            throw new RuntimeException("时间导入格式错误！");
        }
        return Date.from(instant);

    }


    /**
     * Date 转 String
     *
     * @param date
     * @return 日期
     * @author baiban
     */
    public static String DateToStr(Date date) {
        Instant instant = date.toInstant();
        return timeFormat.format(instant);
    }

    /**
     * Date 转 String
     *
     * @param date
     * @return 日期
     * @author baiban
     */
    public static String DateToShortStr(Date date) {
        Instant instant = date.toInstant();
        return dateFormat.format(instant);
    }

    /**
     * Date 转 String
     *
     * @param date
     * @return 日期
     * @author baiban
     */
    public static String DateToStrForWeek(Date date) {
        Instant instant = date.toInstant();
        return dateWeekFormat.format(instant);
    }

    /**
     * Date 转 LocalDateTime
     *
     * @param date
     * @return LocalDateTime
     */
    public static LocalDate dateToLocalDate(Date date) {
        if(date == null)
            return null;
        return dateToLocalDateTime(date).toLocalDate();
    }

    public static LocalTime dateToLocalTime(Date date) {

        return dateToLocalDateTime(date).toLocalTime();
    }

    public static LocalDateTime dateToLocalDateTime(Date date) {
        Instant instant = date.toInstant();
        ZoneId zoneId = ZoneId.systemDefault();
        return instant.atZone(zoneId).toLocalDateTime();
    }

    public static Date localToDate(LocalDateTime localDateTime) {
        ZoneId zoneId = ZoneId.systemDefault();
        ZonedDateTime zdt = localDateTime.atZone(zoneId);
        return Date.from(zdt.toInstant());
    }

    public static Date localDateToDate(LocalDate localDate){
        ZoneId zoneId = ZoneId.systemDefault();
        ZonedDateTime zdt = localDate.atStartOfDay(zoneId);
        Date date = Date.from(zdt.toInstant());
        return date;
    }

    public static Long localToTimestamp(LocalDateTime localDateTime) {
        return localDateTime.toEpochSecond(ZoneOffset.of("+8"));
    }

    public static Date longToDate(long mins){
        LocalDate date = dateToLocalDate(new Date(mins));

        return localDateToDate(date);
    }



    public static Date monthOfFirstDay (Long mins) {
        Calendar cal = Calendar.getInstance();
        cal.setTime(longToDate(mins));
        cal.set(Calendar.DAY_OF_MONTH, 1);
        return cal.getTime();

    }

    public static Date monthOflastDay (Long mins) {
        //获取当前月最后一天
        Calendar ca = Calendar.getInstance();
        ca.setTime(longToDate(mins));
        ca.set(Calendar.DAY_OF_MONTH, ca.getActualMaximum(Calendar.DAY_OF_MONTH));
        ca.add(Calendar.DAY_OF_MONTH, 1);
        return ca.getTime();
    }

    public static Date getBeforMoth(int month){
        LocalDateTime nowTime = LocalDateTime.now();
        LocalDateTime localDateTime = nowTime.plusMonths(month);
        return localToDate(localDateTime);
    }


}

