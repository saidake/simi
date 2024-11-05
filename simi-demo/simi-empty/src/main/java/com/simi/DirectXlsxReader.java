package com.simi;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

import java.io.FileInputStream;
import java.io.IOException;

public class DirectXlsxReader {
    public static void main(String[] args) {
        String filePath = "C:\\Users\\simi\\Desktop\\DevProject\\simi\\simi-test\\simi-empty\\src\\main\\resources\\large_excel_file.xlsx";

        long startTime = System.currentTimeMillis(); // Start timing
        try {
            readFirstLine(filePath);
        } catch (IOException e) {
            e.printStackTrace();
        }
        long endTime = System.currentTimeMillis(); // End timing

        System.out.println("Time taken (direct read): " + (endTime - startTime) + " ms");
    }

    private static void readFirstLine(String filePath) throws IOException {
        try (FileInputStream fis = new FileInputStream(filePath);
             XSSFWorkbook workbook = new XSSFWorkbook(fis)) {

            Sheet sheet = workbook.getSheetAt(0); // Get the first sheet
            Row row = sheet.getRow(0); // Get the first row
            if (row != null) {
                for (Cell cell : row) {
                    switch (cell.getCellType()) {
                        case STRING:
                            System.out.print(cell.getStringCellValue() + "\t");
                            break;
                        case NUMERIC:
                            System.out.print(cell.getNumericCellValue() + "\t");
                            break;
                        case BOOLEAN:
                            System.out.print(cell.getBooleanCellValue() + "\t");
                            break;
                        default:
                            System.out.print("Unknown Cell Type\t");
                            break;
                    }
                }
                System.out.println();
            }
        }
    }
}
