package com.simi;

import com.microsoft.schemas.office.excel.CTClientData;
import com.microsoft.schemas.office.excel.STObjectType;
import com.microsoft.schemas.office.office.STConnectType;
import com.microsoft.schemas.office.office.STInsetMode;
import com.microsoft.schemas.vml.CTGroup;
import com.microsoft.schemas.vml.CTShadow;
import com.microsoft.schemas.vml.CTShape;
import org.apache.poi.ooxml.POIXMLDocumentPart;
import org.apache.poi.schemas.vmldrawing.XmlDocument;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.*;
import org.apache.xmlbeans.XmlCursor;
import org.apache.xmlbeans.XmlException;
import org.apache.xmlbeans.XmlObject;
import org.openxmlformats.schemas.drawingml.x2006.spreadsheetDrawing.CTShapeNonVisual;
import org.openxmlformats.schemas.drawingml.x2006.spreadsheetDrawing.CTTwoCellAnchor;
import org.openxmlformats.schemas.officeDocument.x2006.sharedTypes.STTrueFalse;

import javax.xml.namespace.QName;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.reflect.Field;
import java.math.BigInteger;
import java.util.List;

public class ModifyExcelRow {
    public static void addCheckboxToCell(XSSFSheet sheet) throws NoSuchFieldException, IllegalAccessException {
        XSSFVMLDrawing drawing = null;
        if (sheet.getCTWorksheet().getLegacyDrawing() != null) {
            String legacyDrawingId = sheet.getCTWorksheet().getLegacyDrawing().getId();
            drawing = (XSSFVMLDrawing)sheet.getRelationById(legacyDrawingId);
        } else {
            int drawingNumber = sheet.getPackagePart().getPackage()
                    .getPartsByContentType(XSSFRelation.VML_DRAWINGS.getContentType()).size() + 1;
            POIXMLDocumentPart.RelationPart rp =
                    sheet.createRelationship(XSSFRelation.VML_DRAWINGS, XSSFFactory.getInstance(), drawingNumber, false);
            drawing = rp.getDocumentPart();
            String rId = rp.getRelationship().getId();
            sheet.getCTWorksheet().addNewLegacyDrawing().setId(rId);
        }


        CTGroup grp = (CTGroup)CTGroup.Factory.newInstance();
        CTShape shape = grp.addNewShape();
        shape.setId("_x0000_t201");
        shape.setType("#201" );
        shape.setStyle("position:absolute; visibility:hidden");
        shape.setFillcolor("#ffffe1");
        shape.setInsetmode(STInsetMode.AUTO);
        shape.addNewFill().setColor("#ffffe1");
        CTShadow shadow = shape.addNewShadow();
        shadow.setOn(STTrueFalse.T);
        shadow.setColor("black");
        shadow.setObscured(STTrueFalse.T);
        shape.addNewPath().setConnecttype(STConnectType.NONE);
        shape.addNewTextbox().setStyle("mso-direction-alt:auto");
        CTClientData cldata = shape.addNewClientData();
        cldata.setObjectType(STObjectType.NOTE);
        cldata.addNewMoveWithCells();
        cldata.addNewSizeWithCells();
        cldata.addNewAnchor().setStringValue("1, 15, 0, 2, 3, 15, 3, 16");
        cldata.addNewAutoFill().setStringValue("False");
        cldata.addNewRow().setBigIntegerValue(BigInteger.valueOf(0L));
        cldata.addNewColumn().setBigIntegerValue(BigInteger.valueOf(0L));
        XmlCursor grpCur = grp.newCursor();

        Field rootField =  XSSFVMLDrawing.class.getDeclaredField("root");
        rootField.setAccessible(true);
        XmlDocument root = (XmlDocument)rootField.get(drawing);
        XmlCursor xml = root.getXml().newCursor();
        grpCur.copyXmlContents(xml);


//        com.microsoft.schemas.vml.CTShape shape = CTShape.Factory.newInstance();
//        shape.setId("_x0000_s_x0000_t201");
//        shape.setType("#_x0000_t201");
//        shape.setFillcolor("#ffffe1");
//        CTClientData cldata = shape.addNewClientData();
//        cldata.addNewAnchor().setStringValue("3, 0, 3, 0, 4, 0, 4, 0");
//        cldata.setObjectType(com.microsoft.schemas.office.excel.STObjectType.CHECKBOX);
//        cldata.addTextVAlign("Center");
//        cldata.addChecked(java.math.BigInteger.valueOf(1));
//
//        XmlCursor xmlCursor = shape.newCursor();
//        Field rootField =  XSSFVMLDrawing.class.getDeclaredField("root");
//        rootField.setAccessible(true);
//        XmlDocument root = (XmlDocument)rootField.get(drawing);
//        XmlCursor sourceCursor = root.addNewXml().newCursor();
//        xmlCursor.copyXmlContents(sourceCursor);
//        xmlCursor.close();
    }

    public static void addCheckboxToCell(Sheet sheet, int rowIndex, int colIndex, String checkboxText) {
        // Create a row and cell where the checkbox will be placed
        Row row = sheet.getRow(rowIndex);
        if (row == null) {
            row = sheet.createRow(rowIndex);
        }
        Cell cell = row.getCell(colIndex);
        if (cell == null) {
            cell = row.createCell(colIndex);
        }
        cell.setCellValue(checkboxText);

        // Create a drawing patriarch to hold the checkbox
        XSSFDrawing drawing = (XSSFDrawing) sheet.createDrawingPatriarch();

        // Create an anchor for the checkbox
        XSSFClientAnchor anchor = new XSSFClientAnchor();
        anchor.setCol1(colIndex + 1); // Place checkbox to the right of the cell
        anchor.setRow1(rowIndex);
        anchor.setCol2(colIndex + 2); // Extend checkbox over two columns
        anchor.setRow2(rowIndex + 1); // Extend checkbox over two rows

        // Create a CTShape (Checkbox) and set properties
        try {
            CTShape ctShape = CTShape.Factory.parse(
                    "<xdr:sp xmlns:xdr=\"http://schemas.openxmlformats.org/drawingml/2006/spreadsheetDrawing\">" +
                            "    <xdr:nvSpPr>" +
                            "        <xdr:cNvPr id=\"2\" name=\"Checkbox\"/>" +
                            "        <xdr:cNvSpPr/>" +
                            "    </xdr:nvSpPr>" +
                            "    <xdr:spPr>" +
                            "        <a:xfrm xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\">" +
                            "            <a:off x=\"0\" y=\"0\"/>" +
                            "            <a:ext cx=\"0\" cy=\"0\"/>" +
                            "        </a:xfrm>" +
                            "        <a:prstGeom xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\" prst=\"rect\">" +
                            "            <a:avLst/>" +
                            "        </a:prstGeom>" +
                            "    </xdr:spPr>" +
                            "</xdr:sp>"
            );

            // Set the anchor for the shape
            XSSFClientAnchor shapeAnchor = new XSSFClientAnchor();
            shapeAnchor.setCol1(anchor.getCol1());
            shapeAnchor.setRow1(anchor.getRow1());
            shapeAnchor.setCol2(anchor.getCol2());
            shapeAnchor.setRow2(anchor.getRow2());

            // Add the shape to the drawing
            XSSFSimpleShape shape = drawing.createSimpleShape(shapeAnchor);
            shape.setShapeType(ShapeTypes.RECT);
            shape.getCTShape().set(ctShape);
            shape.setText("☐"); // Unicode character for an empty checkbox

        } catch (XmlException e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        String filePath = "C:\\Users\\simi\\Desktop\\DevProject\\simi\\simi-test\\simi-empty\\src\\main\\resources\\SGZ.xlsx";
        int sheetIndex = 0; // Index of the sheet you want to modify
        int rowIndex = 1;   // Index of the row you want to modify (0-based)
        int cellIndex = 2;  // Index of the cell you want to modify (0-based)
        String newValue = "New Value"; // New value for the cell

        try {
            // Load the Excel file
            FileInputStream fis = new FileInputStream(filePath);
            XSSFWorkbook workbook = new XSSFWorkbook(fis);
            XSSFSheet sheet = workbook.getSheetAt(sheetIndex);
            addCheckboxToCell(sheet);
            // Get the row you want to modify (create it if it doesn't exist)
            Row row = sheet.getRow(rowIndex);
            if (row == null) {
                row = sheet.createRow(rowIndex);
            }

            // Get the cell you want to modify (create it if it doesn't exist)
            Cell cell = row.getCell(cellIndex);
            if (cell == null) {
                cell = row.createCell(cellIndex);
            }

            // Set the new value for the cell
            cell.setCellValue(newValue);

            // Save the changes to the file
            fis.close();
            FileOutputStream fos = new FileOutputStream(filePath);
            workbook.write(fos);
            workbook.close();
            fos.close();

            System.out.println("Excel file modified successfully.");
        } catch (IOException | NoSuchFieldException | IllegalAccessException e) {
            e.printStackTrace();
        }
    }
}
