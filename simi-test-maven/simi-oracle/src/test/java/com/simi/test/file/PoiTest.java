package com.simi.test.file;

import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.zip.ZipInputStream;

public class PoiTest {
    public static void main(String[] args) throws IOException {
        FileInputStream fileInputStream = new FileInputStream(
                "C:\\Users\\saidake\\Desktop\\DevProject\\simi\\simi-service\\simi-oracle\\assets\\test.zip");
        ZipInputStream zipInputStream = new ZipInputStream(
                fileInputStream
        );
        XSSFWorkbook workbook = new XSSFWorkbook(
                new ByteArrayInputStream(zipInputStream.readAllBytes())
                );
        XSSFSheet test = workbook.getSheet("test");
        XSSFRow row = test.getRow(2);
        XSSFCell cell = row.getCell(2);
        cell.setCellValue("test!!分催促");
        workbook.write(new FileOutputStream("C:\\Users\\saidake\\Desktop\\DevProject\\simi\\simi-service\\simi-oracle\\assets\\test-out.xlsx"));
    }
}
