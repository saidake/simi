package com.saidake.common.core.util.data;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
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
     * 日期格式化 日期格式为：yyyy-MM-dd
     *
     * @param date 日期
     * @return 返回yyyy-MM-dd格式日期
     */
    public static String format(Date date) {
        return format(date, yyyyMMddHHmmss);
    }

    /**
     * 日期格式化 日期格式为：yyyy-MM-dd
     *
     * @param date    日期
     * @param pattern 格式，如：DateUtils.DATE_TIME_PATTERN
     * @return 返回yyyy-MM-dd格式日期
     */
    public static String format(Date date, String pattern) {
        if (date != null) {
            SimpleDateFormat df = new SimpleDateFormat(pattern);
            return df.format(date);
        }
        return null;
    }

    /**
     * 字符串转换成日期
     *
     * @param strDate 日期字符串
     * @param pattern 日期的格式，如：DateUtils.DATE_TIME_PATTERN
     */
    public static Date stringToDate(String strDate, String pattern) {
        if (StringUtils.isBlank(strDate)) {
            return null;
        }

        DateTimeFormatter fmt = DateTimeFormat.forPattern(pattern);
        return fmt.parseLocalDateTime(strDate).toDate();
    }

    /**
     * 字符串转换成日期
     *
     * @param strDate 日期字符串
     * @param pattern 日期的格式，如：DateUtils.DATE_TIME_PATTERN
     */
    public static String longToDate(Long strDate, String pattern) {

        SimpleDateFormat df = new SimpleDateFormat(pattern);
        return df.format(new Date(strDate));
    }


    /**
     * 根据周数，获取开始日期、结束日期
     *
     * @param week 周期  0本周，-1上周，-2上上周，1下周，2下下周
     * @return 返回date[0]开始日期、date[1]结束日期
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
     * 对日期的【秒】进行加/减
     *
     * @param date    日期
     * @param seconds 秒数，负数为减
     * @return 加/减几秒后的日期
     */
    public static Date addDateSeconds(Date date, int seconds) {
        DateTime dateTime = new DateTime(date);
        return dateTime.plusSeconds(seconds).toDate();
    }

    /**
     * 对日期的【分钟】进行加/减
     *
     * @param date    日期
     * @param minutes 分钟数，负数为减
     * @return 加/减几分钟后的日期
     */
    public static Date addDateMinutes(Date date, int minutes) {
        DateTime dateTime = new DateTime(date);
        return dateTime.plusMinutes(minutes).toDate();
    }

    /**
     * 对日期的【小时】进行加/减
     *
     * @param date  日期
     * @param hours 小时数，负数为减
     * @return 加/减几小时后的日期
     */
    public static Date addDateHours(Date date, int hours) {
        DateTime dateTime = new DateTime(date);
        return dateTime.plusHours(hours).toDate();
    }

    /**
     * 对日期的【天】进行加/减
     *
     * @param date 日期
     * @param days 天数，负数为减
     * @return 加/减几天后的日期
     */
    public static Date addDateDays(Date date, int days) {
        DateTime dateTime = new DateTime(date);
        return dateTime.plusDays(days).toDate();
    }

    /**
     * 对日期的【周】进行加/减
     *
     * @param date  日期
     * @param weeks 周数，负数为减
     * @return 加/减几周后的日期
     */
    public static Date addDateWeeks(Date date, int weeks) {
        DateTime dateTime = new DateTime(date);
        return dateTime.plusWeeks(weeks).toDate();
    }

    /**
     * 对日期的【月】进行加/减
     *
     * @param date   日期
     * @param months 月数，负数为减
     * @return 加/减几月后的日期
     */
    public static Date addDateMonths(Date date, int months) {
        DateTime dateTime = new DateTime(date);
        return dateTime.plusMonths(months).toDate();
    }

    /**
     * 对日期的【年】进行加/减
     *
     * @param date  日期
     * @param years 年数，负数为减
     * @return 加/减几年后的日期
     */
    public static Date addDateYears(Date date, int years) {
        DateTime dateTime = new DateTime(date);
        return dateTime.plusYears(years).toDate();
    }




    /**
     * 返回baseTime后 num * unit 的毫秒数
     */
    public static Long timePlus(Long baseTime, int num, Long unit) {
        return baseTime + num * unit;
    }

    /**
     * 返回baseTime后 num * unit 的毫秒数
     */
    public static Date timePlus(Date baseTime, int num, Long unit) {
        return new Date(baseTime.getTime() + num * unit);
    }

    /**
     * 返回baseTime后 num * unit 的毫秒数
     */
    public static Long timeMinus(Long baseTime, int num, Long unit) {
        return baseTime - num * unit;
    }

    /**
     * 返回baseTime后 num * unit 的毫秒数
     */
    public static Date timeMinus(Date baseTime, int num, Long unit) {
        return new Date(baseTime.getTime() - num * unit);
    }

    synchronized public static String date2String(Date date, String format){
        if(date == null)
            return null;
        SimpleDateFormat sdf = new SimpleDateFormat(format);
        return sdf.format(date);
    }

    synchronized public static Date string2Date(String dateString, String format){
        if(dateString == null)
            return null;
        SimpleDateFormat sdf = new SimpleDateFormat(format);
        Date parse = null;
        try {
            parse = sdf.parse(dateString);
        } catch (ParseException e) {
            log.error("异常信息为：",e);
        }
        return parse;
    }

    /**
     * 获取两个月之间所有月份的集合
     */
    synchronized public static List<String> getMonthBetween(String minDate, String maxDate) {
        ArrayList<String> result = new ArrayList<String>();


        Calendar min = Calendar.getInstance();
        Calendar max = Calendar.getInstance();

        min.setTime(StringToDateFormat(minDate, "yyyy-MM"));
        min.set(min.get(Calendar.YEAR), min.get(Calendar.MONTH), 1);

        max.setTime(StringToDateFormat(maxDate, "yyyy-MM"));
        max.set(max.get(Calendar.YEAR), max.get(Calendar.MONTH), 2);

        Calendar curr = min;
        while (curr.before(max)) {
            result.add(DatetoStringFormat(curr.getTime(), "yyyy-MM"));
            curr.add(Calendar.MONTH, 1);
        }

        return result;
    }

    /**
     * 日期类型转换为string类型
     *
     * @return
     */
    synchronized public static String DatetoString(Date date) {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMM");
        String str = sdf.format(date);
        return str;
    }

    /**
     * 根据给定的日期格式转换日期
     *
     * @param date
     * @param format
     * @return
     */

    synchronized public static String DatetoStringFormat(Date date, String format) {
        if(date == null)
            return null;
        SimpleDateFormat sdf = new SimpleDateFormat(format);
        String str = sdf.format(date);
        return str;
    }


    /**
     * 根据给定的格式字符串转换成date
     *
     * @param str
     * @return
     */
    synchronized public static Date StringToDate(String str) {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
        Date date = null;
        try {
            date = sdf.parse(str);
        } catch (ParseException e) {
            log.error("异常信息为：",e);
        }
        return date;
    }

    /**
     * 根据给定的格式字符串转换成date
     *
     * @param str
     * @return
     */
    synchronized public static Date StringToDateFormat(String str, String format) {
        SimpleDateFormat sdf = new SimpleDateFormat(format);
        Date date = null;
        try {
            date = sdf.parse(str);
        } catch (ParseException e) {
            log.error("DateUtil.StringToDateFormat exception");
            log.error("异常信息为：",e);
        }
        return date;
    }

    /**
     * 获取传入日期的前n个月
     *
     * @param date
     * @param n
     * @return
     */
    synchronized public static Date minusMonth(Date date, int n) {
        log.debug("n:{}", n);
        log.debug("date:{}", DatetoStringFormat(date, "yyyy-MM-dd"));
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(date);
        calendar.set(Calendar.MONTH, calendar.get(Calendar.MONTH) - n);
        Date date1 = calendar.getTime();
        log.debug("date1:{}", DatetoStringFormat(date1, "yyyy-MM-dd"));
        return date1;
    }

    /**
     * 时间戳准换为String类型
     *
     * @param timeStamp
     * @param format
     * @return
     */
    synchronized public static String timeStampToString(long timeStamp, String format) {
        SimpleDateFormat sdf = new SimpleDateFormat(format);
        String str = sdf.format(new Date(timeStamp));
        log.debug("timeStampToString " + str);
        return str;
    }

    /**
     * String类型转换为时间戳
     */
    synchronized public static long datestrToTimestamp(String datestr, String format) {
        Date date = DateUtil.string2Date(datestr, format);
        long time = 0;
        if(date != null)time = date.getTime();
        return time;
    }


    /**
     * 获取当前时间的前一年的时间
     *
     * @return
     */
    synchronized public static Date lastYearDate(Date date) {
        Calendar c = Calendar.getInstance();
        c.setTime(new Date());
        c.add(Calendar.YEAR, -1);
        Date y = c.getTime();
        return y;
    }


    /**
     * 获取当前时间的前一天的时间
     *
     * @return
     */
    synchronized public static Date lastDayDate(Date date) {
        Calendar c = Calendar.getInstance();
        c.setTime(date);
        c.add(Calendar.DAY_OF_MONTH, -1);
        Date y = c.getTime();
        return y;
    }


    /**
     * 时间戳转日期
     * @param ms
     * @return
     */
    synchronized public static Date transForDate(Long ms){
        if(ms==null){
            ms=0L;
        }
        SimpleDateFormat sdf=new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        Date temp=null;
        if(ms!=null){
            try {
                String str=sdf.format(ms);
                temp=sdf.parse(str);
            } catch (ParseException e) {
                log.error("异常信息为：",e);
            }
        }
        return temp;
    }

    public static String getWeekOfDate(Date date) {
        String[] weekOfDays = {"星期日", "星期一", "星期二", "星期三", "星期四", "星期五", "星期六"};
        Calendar calendar = Calendar.getInstance();
        if (date != null) {
            calendar.setTime(date);
        }
        int w = calendar.get(Calendar.DAY_OF_WEEK) - 1;
        if (w < 0) {
            w = 0;
        }
        return weekOfDays[w];
    }

    synchronized public static Date getStartTime(Long time) {
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(new Date(time));
        calendar.set(Calendar.HOUR_OF_DAY, 0);
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        return calendar.getTime();
    }

    synchronized public static Date getEndTime(Long time) {
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(new Date(time));
        calendar.set(Calendar.HOUR_OF_DAY, 23);
        calendar.set(Calendar.MINUTE, 59);
        calendar.set(Calendar.SECOND, 59);
        calendar.set(Calendar.MILLISECOND, 999);
        return calendar.getTime();
    }

    /**
     * 计算月份数
     */
    public static int getMonthAmount(Long startTime, Long endTime) {
        LocalDate startDate = LocalDateTimeUtil.dateToLocalDate(new Date(startTime));
        LocalDate endDate = LocalDateTimeUtil.dateToLocalDate(new Date(endTime));
        return (endDate.getYear() - startDate.getYear()) * 12 + (endDate.getMonthValue() - startDate.getMonthValue());
    }

    /**
     * 计算整月天数
     */
    public static int getDayAmount(LocalDate startDate, LocalDate endDate) {
        int count = 0;
        LocalDate temp = startDate;
        while (!temp.isAfter(endDate)) {
            count += temp.lengthOfMonth();
            temp = temp.plusMonths(1);
        }
        return count;
    }

    /**
     * 计算非整月天数
     */
    public static int getDayAmount1(LocalDate startDate, LocalDate endDate) {
        int count = 0;
        while (!startDate.isEqual(endDate)) {
            startDate = startDate.plusDays(1);
            count ++;
        }
        return count+1;
    }

    public static Map<String,Date> beginAndLastDate(String date){
        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMdd");
        long time = 0l;
        try {
            Date todayDate = sdf.parse(date);
            time = todayDate.getTime();
        } catch (ParseException e) {
            log.error("异常信息为：",e);
        }
        Date endTime = getEndTime(time);
        Date startTime = getStartTime(time);
        Map<String,Date> map = new HashMap<>();
        map.put("startTime",startTime);
        map.put("endTime",endTime);
        return  map;
    }

    public static Date getStartTime(Date todayDate){
        Calendar todayStart = Calendar.getInstance();
        todayStart.setTime(todayDate);
        todayStart.set(Calendar.HOUR, 0);
        todayStart.set(Calendar.MINUTE, 0);
        todayStart.set(Calendar.SECOND, 0);
        todayStart.set(Calendar.MILLISECOND, 0);
        return todayStart.getTime();
    }

    public static Date getEndTime(Date todayDate){
        Calendar todayEnd = Calendar.getInstance();
        todayEnd.setTime(todayDate);
        todayEnd.set(Calendar.HOUR, 23);
        todayEnd.set(Calendar.MINUTE, 59);
        todayEnd.set(Calendar.SECOND, 59);
        todayEnd.set(Calendar.MILLISECOND, 59);
        return todayEnd.getTime();
    }

}
