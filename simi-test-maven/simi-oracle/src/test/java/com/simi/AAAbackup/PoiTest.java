package com.simi.AAAbackup;
import java.io.*;

import com.microsoft.schemas.office.excel.STObjectType;
import org.apache.poi.openxml4j.opc.*;
import org.apache.poi.ooxml.POIXMLDocumentPart;
import org.apache.poi.schemas.vmldrawing.CTXML;
import org.apache.poi.schemas.vmldrawing.XmlDocument;
import org.apache.poi.ss.usermodel.ShapeTypes;
import org.apache.xmlbeans.*;

import org.apache.poi.xssf.usermodel.*;

import com.microsoft.schemas.vml.*;
import com.microsoft.schemas.office.excel.CTClientData;
import org.apache.xmlbeans.impl.store.Cursor;
import org.openxmlformats.schemas.drawingml.x2006.main.CTShapeStyle;
import org.openxmlformats.schemas.drawingml.x2006.main.STPresetLineDashVal;
import org.openxmlformats.schemas.drawingml.x2006.main.STShapeType;
import org.openxmlformats.schemas.drawingml.x2006.spreadsheetDrawing.CTDrawing;
import org.openxmlformats.schemas.officeDocument.x2006.sharedTypes.STTrueFalse;
import org.openxmlformats.schemas.spreadsheetml.x2006.main.CTLegacyDrawing;

import java.lang.reflect.Field;
import javax.xml.namespace.QName;

import java.util.List;

public class PoiTest {

    private static XSSFVMLDrawing getVMLDrawing(XSSFSheet sheet) throws Exception {
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
        return drawing;
    }

    private static void addCheckboxShapetype(XSSFVMLDrawing drawing) throws Exception {
        String shapeTypeId = "_x0000_t201";
        CTShapetype shapetype = CTShapetype.Factory.newInstance();
        shapetype.setId(shapeTypeId);
        shapetype.setCoordsize("21600,21600");
        shapetype.setSpt(201);
        shapetype.setPath2("m,l,21600r21600,l21600,xe");

        Field _items = XSSFVMLDrawing.class.getDeclaredField("_items");
        _items.setAccessible(true);
        @SuppressWarnings("unchecked") //we know the problem and expect runtime error if it possibly occurs
                List<XmlObject> items = (List<XmlObject>)_items.get(drawing);

        Field _qnames = XSSFVMLDrawing.class.getDeclaredField("_qnames");
        _qnames.setAccessible(true);
        @SuppressWarnings("unchecked") //we know the problem and expect runtime error if it possibly occurs
                List<QName> qnames = (List<QName>)_qnames.get(drawing);

        items.add(shapetype);
        qnames.add(new QName("urn:schemas-microsoft-com:vml", "shapetype"));
    }

    private static void addCheckbox(XSSFVMLDrawing drawing,
                                    int col1, int dx1, int row1, int dy1, int col2, int dx2, int row2, int dy2,
                                    String label, boolean checked) throws Exception {

        String shapeTypeId = "_x0000_t201";

        Field _shapeId = XSSFVMLDrawing.class.getDeclaredField("_shapeId");
        _shapeId.setAccessible(true);
        int shapeId = (int)_shapeId.get(drawing);
        _shapeId.set(drawing, shapeId + 1);

        CTShape shape = CTShape.Factory.newInstance();
        shape.setId("_x0000_s" + shapeId);
        shape.setType("#" + shapeTypeId);
//        shape.setFilled(com.microsoft.schemas.vml.STTrueFalse.F);
//        shape.setStroked(com.microsoft.schemas.vml.STTrueFalse.F);
        String textboxHTML =
                "<div style='text-align:left'>"
                        +"<font face=\"Tahoma\" size=\"160\" color=\"auto\">" + label + "</font>"
                        +"</div>";
        CTTextbox[] textboxArray = new CTTextbox[1];
        textboxArray[0] = CTTextbox.Factory.parse(textboxHTML);
        textboxArray[0].setStyle("mso-direction-alt:auto");
//        textboxArray[0].setSingleclick(com.microsoft.schemas.office.office.STTrueFalse.F);
        shape.setTextboxArray(textboxArray);
        CTClientData cldata = shape.addNewClientData();
        cldata.setObjectType(com.microsoft.schemas.office.excel.STObjectType.CHECKBOX);
        cldata.addNewMoveWithCells();
        cldata.addNewSizeWithCells();
        cldata.addNewAnchor().setStringValue(
                "" + col1 + ", " + dx1 + ", " + row1 + ", " +dy1 + ", " + col2 + ", " + dx2 + ", " + row2 + ", " + dy2
        );
//        cldata.addAutoFill(com.microsoft.schemas.office.excel.STTrueFalseBlank.FALSE);
//        cldata.addAutoLine(com.microsoft.schemas.office.excel.STTrueFalseBlank.FALSE);
        cldata.addTextVAlign("Center");
//        cldata.addNoThreeD(com.microsoft.schemas.office.excel.STTrueFalseBlank.TRUE);

        cldata.addChecked((checked)?java.math.BigInteger.valueOf(1):java.math.BigInteger.valueOf(0));

        Field _items = XSSFVMLDrawing.class.getDeclaredField("_items");
        _items.setAccessible(true);
        @SuppressWarnings("unchecked") //we know the problem and expect runtime error if it possibly occurs
                List<XmlObject> items = (List<XmlObject>)_items.get(drawing);

        Field _qnames = XSSFVMLDrawing.class.getDeclaredField("_qnames");
        _qnames.setAccessible(true);
        @SuppressWarnings("unchecked") //we know the problem and expect runtime error if it possibly occurs
                List<QName> qnames = (List<QName>)_qnames.get(drawing);

        items.add(shape);
        qnames.add(new QName("urn:schemas-microsoft-com:vml", "shape"));

    }

    public static void main(String[] args) throws Exception {

        XSSFWorkbook workbook  = new XSSFWorkbook();

        //following is necessary to be textboxHTML of the CTShape compatible with Excel 2007.
        //<fileVersion appName="xl" lastEdited="4" lowestEdited="0" rupBuild="4507"/>
//        workbook.getCTWorkbook().addNewFileVersion().setAppName("xl");
//        workbook.getCTWorkbook().getFileVersion().setLastEdited("4");
//        workbook.getCTWorkbook().getFileVersion().setLowestEdited("0");
//        workbook.getCTWorkbook().getFileVersion().setRupBuild("4507");

        XSSFSheet sheet = workbook.createSheet();
        XSSFCell cell = sheet.createRow(5).createCell(5);
//  XSSFDrawing drawing = sheet.createDrawingPatriarch();
//  XSSFClientAnchor anchor = workbook.getCreationHelper().createClientAnchor();
//  anchor.setCol1(cell.getColumnIndex());
//  anchor.setCol2(cell.getColumnIndex()+1);
//  anchor.setRow1(cell.getRow().getRowNum());
//  anchor.setRow2(cell.getRow().getRowNum()+3);
//  XSSFComment comment = drawing.createCellComment(anchor);
//  XSSFRichTextString str = workbook.getCreationHelper().createRichTextString("Hello, World!");
//  comment.setString(str);
//  comment.setAuthor("Apache POI");
//  cell.setCellComment(comment);


//        XSSFDrawing drawingPatriarch = sheet.createDrawingPatriarch();
//        XSSFSimpleShape simpleShape = drawingPatriarch.createSimpleShape(new XSSFClientAnchor(0, 0, 1, 1, 0, 0, 1, 1));
//        XSSFVMLDrawing vmlDrawing = getVMLDrawing(sheet);
//        addCheckboxShapetype(vmlDrawing);
//        addCheckbox(vmlDrawing, 1, 0, 1, 0, 3, 0, 2, 0, "Checkbox 1", true);
//        addCheckbox(vmlDrawing, 1, 0, 2, 0, 3, 0, 3, 0, "Checkbox 2", false);
        //A. get drawing
//        int drawingNumber = sheet.getPackagePart().getPackage()
//                .getPartsByContentType(XSSFRelation.VML_DRAWINGS.getContentType()).size() + 1;
//        POIXMLDocumentPart.RelationPart rp =
//                sheet.createRelationship(XSSFRelation.VML_DRAWINGS, XSSFFactory.getInstance(), drawingNumber, false);
        XSSFVMLDrawing drawing=getVMLDrawing(sheet);
        //A. set shapeId
        Field _shapeId = XSSFVMLDrawing.class.getDeclaredField("_shapeId");
        _shapeId.setAccessible(true);
        int shapeId = (int)_shapeId.get(drawing);
        _shapeId.set(drawing, shapeId + 1);
        //A. set shape
        CTShape shape = CTShape.Factory.newInstance();
        shape.setId("_x0000_s" + shapeId);
        String shapeTypeId = "_x0000_t201";
        shape.setType("#" + shapeTypeId);
        shape.setFilled(STTrueFalse.F);
        shape.setStroked(STTrueFalse.F);
        String textboxHTML =
                "<div style='text-align:left'>"
                        +"<font face=\"Tahoma\" size=\"160\" color=\"auto\">" + "test" + "</font>"
                        +"</div>";
        CTTextbox[] textboxArray = new CTTextbox[1];
        textboxArray[0] = CTTextbox.Factory.parse(textboxHTML);
        textboxArray[0].setStyle("mso-direction-alt:auto");
        textboxArray[0].setSingleclick(STTrueFalse.F);
        shape.setTextboxArray(textboxArray);

        CTClientData ctClientData = shape.addNewClientData();
        ctClientData.setObjectType(com.microsoft.schemas.office.excel.STObjectType.CHECKBOX);
        ctClientData.addNewMoveWithCells();
        ctClientData.addNewSizeWithCells();
        ctClientData.addNewAnchor().setStringValue(
                "" + 0 + ", " + 0 + ", " + 1 + ", " +1 + ", " + 0 + ", " + 0 + ", " + 2 + ", " + 2
        );
        ctClientData.addTextVAlign("Center");
        ctClientData.addChecked(java.math.BigInteger.valueOf(1));

        //A. add shape
        Field root = XSSFVMLDrawing.class.getDeclaredField("root");
        root.setAccessible(true);
        XmlDocument xmlDocument = (XmlDocument)root.get(drawing);
        CTXML ctxml = xmlDocument.addNewXml();
        ctxml.set(shape);

        FileOutputStream out = new FileOutputStream("D:\\Desktop\\DevProject\\simi\\sdk-service\\sdk-generator\\src\\test\\java\\com\\saidake\\Excel.xlsx");
        workbook.write(out);
        out.close();
        workbook.close();

    }

}