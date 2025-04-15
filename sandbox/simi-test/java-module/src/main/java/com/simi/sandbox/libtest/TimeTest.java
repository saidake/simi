package com.simi.sandbox.libtest;

import lombok.extern.slf4j.Slf4j;

import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

@Slf4j
public class TimeTest {
    public static void main(String[] args) {
        Timestamp timestamp = new Timestamp(System.currentTimeMillis());
        System.out.println(timestamp.toLocalDateTime().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS")));  // Correctly displays milliseconds
        String dateString1 = "2024-02-05";
        String dateString2 = "2024-02-5";

        // Use the pattern to match both date formats
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-M-dd");

        LocalDate date1 = LocalDate.parse(dateString1, formatter);
        LocalDate date2 = LocalDate.parse(dateString2, formatter);

        log.info("Parsed date 1: {}", date1);  // Output: 2024-02-05
        log.info("Parsed date 2: {}", date2);  // Output: 2024-02-05

    }
}
