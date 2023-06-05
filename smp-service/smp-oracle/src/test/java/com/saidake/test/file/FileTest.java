package com.saidake.test.file;


import com.saidake.common.core.util.file.SmpInit;
import org.xml.sax.SAXException;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerException;
import java.io.IOException;

public class FileTest {
    public static void main(String[] args) throws IOException, ParserConfigurationException, SAXException, TransformerException {
        SmpInit.init();
    }
}
