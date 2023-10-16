package com.simi;



import cn.hutool.core.codec.Base64;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.PDPageContentStream;
import org.apache.pdfbox.pdmodel.common.PDRectangle;
import org.apache.pdfbox.pdmodel.graphics.image.LosslessFactory;
import org.apache.pdfbox.pdmodel.graphics.image.PDImageXObject;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;



//    const imageData = ctx.getImageData(0, 0, canvas.width, canvas.height);
//    const pixels = imageData.data;
//    for (let i = 3, n = canvas.width * canvas.height * 4; i < n; i += 4) {
//        pixels[i] = pixels[i] < 127? 0 : 255
//    }
//     ctx.putImageData(imageData, 0, 0);
public class Main {

    public static void main(String[] args) {
        try{
            //get file
            File outputFile=new File("C:\\Users\\simi\\Desktop\\DevProject\\saidake-manage-project\\simi-test\\simi-empty\\src\\main\\java\\com\\simi\\test.pdf");
            String str= Files.readString(Path.of("C:\\Users\\simi\\Desktop\\DevProject\\saidake-manage-project\\simi-test\\simi-empty\\src\\main\\java\\com\\simi\\input-byte-string.txt"), StandardCharsets.UTF_8);
            byte[] decode = Base64.decode(str);

            PDDocument pdfDocument = new PDDocument();
            PDPage page = new PDPage(new PDRectangle(1920,543));
            pdfDocument.addPage(page);

            PDPageContentStream contentStream = new PDPageContentStream(pdfDocument, page);
            PDImageXObject fromByteArray = PDImageXObject.createFromByteArray(pdfDocument, decode, null);
            contentStream.drawImage(fromByteArray, 0, 0);
            contentStream.close();

            pdfDocument.save(outputFile);
            pdfDocument.close();
            System.out.println("success");
        } catch (Exception io){
            System.out.println(" -- fail --" + io);
        }

    }
}
