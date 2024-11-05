package com.simi;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

import java.io.FileOutputStream;
import java.io.IOException;

public class LargeExcelFileGenerator {

    public static void main(String[] args) {
        String filePath = "C:\\Users\\simi\\Desktop\\DevProject\\simi\\simi-test\\simi-empty\\src\\main\\resources\\large_excel_file.xlsx";
        int numberOfRows = 100000;  // Adjust this number to achieve the desired file size
        int numberOfColumns = 10;   // Number of columns

        try {
            generateLargeExcelFile(filePath, numberOfRows, numberOfColumns);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static void generateLargeExcelFile(String filePath, int numberOfRows, int numberOfColumns) throws IOException {
        Workbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet("LargeSheet");

        for (int rowNum = 0; rowNum < numberOfRows; rowNum++) {
            Row row = sheet.createRow(rowNum);
            for (int colNum = 0; colNum < numberOfColumns; colNum++) {
                Cell cell = row.createCell(colNum);
                cell.setCellValue("Data" + rowNum + "_" + colNum);  // Fill with sample data
            }
        }

        try (FileOutputStream fileOut = new FileOutputStream(filePath)) {
            workbook.write(fileOut);
        }

        workbook.close();
        System.out.println("Excel file generated: " + filePath);
    }
}
