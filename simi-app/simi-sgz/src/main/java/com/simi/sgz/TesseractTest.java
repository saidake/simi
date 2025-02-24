package com.simi.sgz;

import com.simi.sgz.AAAconfig.SgzConstants;
import lombok.extern.slf4j.Slf4j;
import net.sourceforge.tess4j.Tesseract;
import net.sourceforge.tess4j.TesseractException;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

@Slf4j
public class TesseractTest {
//        coordinates.add(copyAndIncrement(coordinate,905,0));
//        coordinates.add(copyAndIncrement(coordinate,0,525));
//        coordinates.add(copyAndIncrement(coordinate,905,525));
    public static void main(String[] args) throws AWTException, TesseractException, IOException {
        Robot robot=new Robot();
        int x1=649+905;
        int y1=163;

        int x2=655+905;
        int y2=167;
        Rectangle captureRect = new Rectangle(x1, y1, (x2 - x1)*3, (y2 - y1)*3);
        BufferedImage screenCapture = robot.createScreenCapture(captureRect);
        // Define the output file
        File outputFile = new File("C:\\Users\\simi\\Desktop\\DevProjects\\simi\\simi-app\\simi-sgz\\src\\main\\java\\com\\simi\\sgz\\screenshot.png"); // Change the path as needed
        // Save the BufferedImage as a PNG file
        ImageIO.write(screenCapture, "png", outputFile);
        // Initialize Tesseract
        Tesseract tesseract = new Tesseract();
        tesseract.setDatapath(SgzConstants.TESSERACT_ENGIN_PATH); // Path to Tesseract's tessdata directory
        // Perform OCR
        String result = tesseract.doOCR(screenCapture);
        log.info("Detected number: {}", result);
        log.info("Detected number: {}", result.trim().replaceAll("\\D+",""));
    }
}
