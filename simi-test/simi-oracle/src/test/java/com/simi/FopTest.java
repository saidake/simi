package com.simi;

import org.apache.fop.apps.*;

import javax.xml.transform.*;
import javax.xml.transform.sax.SAXResult;
import javax.xml.transform.stream.StreamSource;
import java.io.*;

public class FopTest {
    public static void main(String[] args) throws IOException {
        // Setup directories
        File baseDir = new File(".");
        File outDir = new File(baseDir, "out");
        outDir.mkdirs();

        File xmlfile = new File(baseDir, "xml/xml/projectteam.xml");
        File xsltfile = new File(baseDir, "xml/xslt/projectteam2fo.xsl");
        File pdffile = new File(outDir, "ResultXML2PDF.pdf");

        final FopFactory fopFactory = FopFactory.newInstance(new File(".").toURI());
        FOUserAgent foUserAgent = fopFactory.newFOUserAgent();
        OutputStream out = new FileOutputStream(pdffile);
        out = new BufferedOutputStream(out);

        try {
            // Construct fop with desired output format
            Fop fop = fopFactory.newFop(MimeConstants.MIME_PDF, foUserAgent, out);

            // Setup XSLT
            TransformerFactory factory = TransformerFactory.newInstance();
            Transformer transformer = factory.newTransformer(new StreamSource(xsltfile));   //开始处理 原始xsl文件

            // Set the value of a <param> in the stylesheet
            transformer.setParameter("versionParam", "2.0");

            // Setup input for XSLT transformation（数据对象，可以是对象）
            Source src = new StreamSource(xmlfile);

            // Resulting SAX events (the generated FO) must be piped through to FOP
            Result res = new SAXResult(fop.getDefaultHandler());

            // Start XSLT transformation and FOP processing
            transformer.transform(src, res);
        } catch (FOPException | TransformerException e) {
            throw new RuntimeException(e);
        } finally {
            out.close();
        }

    }
}
