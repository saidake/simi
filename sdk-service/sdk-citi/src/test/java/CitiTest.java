import com.opencsv.CSVReader;
import com.opencsv.exceptions.CsvException;
import com.saidake.citi.domain.file.SdkSheet;
import com.saidake.citi.domain.file.SdkWorkbook;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.Getter;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
public class CitiTest {
    @Data
    private static class Person {
        String name;
        Integer age;
        Son son;
    }
    @Data
    private static class Son {
        String sonName;
        Integer sonAge;
    }
    @AllArgsConstructor
    @Getter
    private enum ZZ {
        _3("GP-3"),
        _4("GP-4");
        private String value;
    }
    public static void main(String[] args) throws IOException, CsvException {
        FileInputStream fileInputStream=new FileInputStream("D:\\Desktop\\DevProject\\saidake-manage-project\\sdk-service\\sdk-citi\\assets\\test.xlsx");
        // sdk
        SdkWorkbook sdkWorkbook=new SdkWorkbook(fileInputStream);
        SdkSheet sdkSheet = sdkWorkbook.getSheet("综合账号结算报告");
        // poi
        XSSFWorkbook xssfWorkbook=new XSSFWorkbook(fileInputStream);
        // OPCPackage.open(stream)

        XSSFSheet xssfSheet = xssfWorkbook.getSheet("综合账号结算报告");
        XSSFRow xssfRow = xssfSheet.getRow(13);
        XSSFCell xssfCell = xssfRow.getCell(1);
        System.out.println(xssfCell.getStringCellValue());

        CSVReader csvReader=new CSVReader(new FileReader("D:\\Desktop\\DevProject\\saidake-manage-project\\sdk-service\\sdk-citi\\assets\\EURUSD\\EURUSD5.csv"));
        String[] strings = csvReader.readNext();
        System.out.println(strings[0]);
        System.out.println(strings[1]);
        System.out.println(strings[2]);
        System.out.println(strings[3]);


    }
}