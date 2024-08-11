package com.simi;

import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

import java.io.*;

public class ChunkedExcelReader {

    public static void main(String[] args) {
        String filePath = "C:\\Users\\simi\\Desktop\\DevProject\\simi\\simi-test\\simi-empty\\src\\main\\resources\\large_excel_file.xlsx";
        int chunkSize = 1024 * 1024; // Chunk size in bytes (e.g., 1 MB)

        long startTime = System.currentTimeMillis(); // Start timing
        try {
            processFileInChunks(filePath, chunkSize);
        } catch (IOException e) {
            e.printStackTrace();
        }
        long endTime = System.currentTimeMillis(); // End timing

        System.out.println("Time taken (chunked read): " + (endTime - startTime) + " ms");
    }

    private static void processFileInChunks(String filePath, int chunkSize) throws IOException {
        try (BufferedInputStream bis = new BufferedInputStream(new FileInputStream(filePath))) {
            byte[] buffer = new byte[chunkSize];
            int bytesRead;
            while ((bytesRead = bis.read(buffer)) != -1) {
                try (ByteArrayInputStream bais = new ByteArrayInputStream(buffer, 0, bytesRead);
                     XSSFWorkbook workbook = new XSSFWorkbook(bais)) {

                    // Process workbook (e.g., read data from sheets)
                    Sheet sheet = workbook.getSheetAt(0); // Get the first sheet
                    processSheet(sheet);
                }
            }
        }
    }

    private static void processSheet(Sheet sheet) {
        for (Row row : sheet) {
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
