package com.simi.sandbox.libtest;

import java.sql.Timestamp;
import java.time.format.DateTimeFormatter;

public class TimeTest {
    public static void main(String[] args) {
        Timestamp timestamp = new Timestamp(System.currentTimeMillis());
        System.out.println(timestamp.toLocalDateTime().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS")));  // Correctly displays milliseconds
    }
}
