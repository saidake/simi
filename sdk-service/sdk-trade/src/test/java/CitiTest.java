import com.saidake.trade.domain.file.SdkSheet;
import com.saidake.trade.domain.file.SdkWorkbook;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.Getter;
import org.apache.poi.xssf.usermodel.XSSFCell;
import org.apache.poi.xssf.usermodel.XSSFRow;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import java.io.*;

public class CitiTest {
    @Data
    private static class Father extends Person{
        String name;
    }
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
    public static void main(String[] args) throws IOException {
        Person  f=new Father();
        FileInputStream fileInputStream=new FileInputStream("D:\\Desktop\\DevProject\\saidake-manage-project\\sdk-service\\sdk-trade\\assets\\xlsx\\test.xlsx");
        //============================================================================= sdk
        SdkWorkbook sdkWorkbook=new SdkWorkbook(fileInputStream);
        SdkSheet sdkSheet = sdkWorkbook.getSheet("综合账号结算报告");
        //============================================================================= poi
        XSSFWorkbook xssfWorkbook=new XSSFWorkbook(fileInputStream);
        XSSFSheet xssfSheet = xssfWorkbook.getSheet("综合账号结算报告");
        XSSFRow xssfRow = xssfSheet.getRow(13);
        XSSFCell xssfCell = xssfRow.getCell(1);
        System.out.println(xssfCell.getStringCellValue());
    }
}