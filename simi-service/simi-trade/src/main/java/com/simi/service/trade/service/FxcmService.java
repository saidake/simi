package com.simi.service.trade.service;

import com.simi.service.trade.service.sax.FxcmAccountParser;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.filefilter.EmptyFileFilter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;

@Service
public class FxcmService {

    @Value("${fxcm.account-info-path}")
    private String fxcmDirectory;

    public void getFxcmAccountInfo() throws IOException, SAXException, ParserConfigurationException {
        Collection<File> fileCollection = FileUtils.listFiles(new File(fxcmDirectory), EmptyFileFilter.NOT_EMPTY, null);
        Assert.isTrue(fileCollection.iterator().hasNext(),"No account files found");
        File accountFile = fileCollection.iterator().next();
        this.preprocessAccountFile(accountFile);
        SAXParserFactory saxParserFactory = SAXParserFactory.newInstance();
        SAXParser saxParser = saxParserFactory.newSAXParser();
        FxcmAccountParser fxcmAccountParser = new FxcmAccountParser();
        saxParser.parse(accountFile, fxcmAccountParser);
    }

    private void preprocessAccountFile(File accountFile) throws IOException {
        String fileContent = Files.readString(Path.of(accountFile.toString()));
        String checkFileEndString="</ss:Workbook>";
        if(!fileContent.endsWith(checkFileEndString)){
            fileContent=fileContent.substring(0,fileContent.lastIndexOf(checkFileEndString)+checkFileEndString.length());
            Files.writeString(Path.of(accountFile.toString()), fileContent);
        }
    }

}
