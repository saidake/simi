package com.simi.test.data;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;

public class DateTest {
    public static void main(String[] args) {
        Calendar instance = Calendar.getInstance();
        instance.add(Calendar.DAY_OF_MONTH,22);
        Date time = instance.getTime();
        System.out.println(new SimpleDateFormat("d MMMMM yyyy", Locale.ENGLISH).format(time));
    }
}
