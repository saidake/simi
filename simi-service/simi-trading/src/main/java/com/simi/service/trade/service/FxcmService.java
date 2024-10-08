package com.simi.service.trade.service;

import com.simi.service.trade.domain.Account;
import com.simi.service.trade.domain.AccountResponse;
import com.simi.service.trade.service.sax.FxcmAccountParser;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.filefilter.EmptyFileFilter;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;

import javax.xml.parsers.*;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Service
public class FxcmService {

    @Value("${fxcm.account-info-path}")
    private String fxcmDirectory;

    public AccountResponse retrieveFxcmAccountInfo() throws XPathExpressionException, ParserConfigurationException, IOException, SAXException {
        // remove error char
        File file = new File(fxcmDirectory);
        File[] files = file.listFiles();
        assert files != null;
        File fuhuiFile=files[0];
        BufferedReader bufferedReader = new BufferedReader(new FileReader(fuhuiFile));
        List<String> stringList = new ArrayList<>();
        String currentLine = bufferedReader.readLine();
        while (null != currentLine) {
            stringList.add(currentLine);
            currentLine = bufferedReader.readLine();
        }
        String lastLine = stringList.get(stringList.size() - 1);
        Pattern compile = Pattern.compile("(?<=</ss:Workbook>).+$");
        Matcher matcher = compile.matcher(lastLine);
        boolean hasErrorChar=false;
        while (matcher.find()){
            hasErrorChar=true;
        }
        if(hasErrorChar){
            String substring = lastLine.substring(0, lastLine.length() - 1);
            stringList.set(stringList.size()-1,substring);
        }
        bufferedReader.close();
        BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(fuhuiFile));
        for (String currentWriteLine : stringList) {
            bufferedWriter.write(currentWriteLine);
        }
        bufferedWriter.close();
        // start read document
        DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();
        DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();
        Document document = documentBuilder.parse(fuhuiFile);
        XPathFactory xpathFactory=XPathFactory.newInstance();
        XPath xpath=xpathFactory.newXPath();
        NodeList nodeList=(NodeList) xpath.evaluate("//Worksheet/Table/Row", document, XPathConstants.NODESET);
        LinkedList<Account> accountLinkedList=new LinkedList<>();
        AccountResponse accountResponse=new AccountResponse();
        //START//========================================================================================== foreach nodeList
        for(int nodeInd=0,rowInd=0,cellBlock=0;nodeInd<nodeList.getLength();nodeInd++) {
            Node item = nodeList.item(nodeInd);
            if(item.getNodeType()==Node.ELEMENT_NODE){
                //time
                if(rowInd==2){
                    String totalTime=(String) xpath.evaluate("Cell[2]/Data", item,XPathConstants.STRING);
                    accountResponse.setTotalTime(totalTime);
                }
                if(rowInd>=13){
                    if(cellBlock==1){
                        Account account =  accountLinkedList.getLast();
                        //结束时间：2022/7/14 7:26
                        account.setEndTime((String) xpath.evaluate("Cell[4]/Data", item,XPathConstants.STRING));
                        if(account.getIsBuy()){
                            //卖出：1.0007200000000001
                            account.setSellingPrice((String) xpath.evaluate("Cell[5]/Data", item,XPathConstants.STRING));
                        }else{
                            //买进：1.0007200000000001
                            account.setPurchasePrice((String) xpath.evaluate("Cell[6]/Data", item,XPathConstants.STRING));
                        }
                        //总盈/亏：1.0007200000000001
                        account.setTotalProfitAndLoss((String) xpath.evaluate("Cell[7]/Data", item,XPathConstants.STRING));
                        //净盈/亏：1.0007200000000001
                        account.setNetProfitAndLoss((String) xpath.evaluate("Cell[12]/Data", item,XPathConstants.STRING));
                        cellBlock=0;
                    }else{
                        String title=(String) xpath.evaluate("Cell[1]/Data", item,XPathConstants.STRING);
                        if("总和:".equals(title))break;
                        Account account = new Account();
                        //交易货币：EUR/USD
                        account.setCurrencyType((String) xpath.evaluate("Cell[2]/Data", item,XPathConstants.STRING));
                        //成交量：1000
                        account.setTurnover((String) xpath.evaluate("Cell[3]/Data", item,XPathConstants.STRING));
                        //开始时间：2022/7/14 7:26
                        account.setStartTime((String) xpath.evaluate("Cell[4]/Data", item,XPathConstants.STRING));
                        //卖出：1.0007200000000001
                        String sellingPrice=(String) xpath.evaluate("Cell[5]/Data", item,XPathConstants.STRING);
                        account.setIsBuy(StringUtils.isBlank(sellingPrice));
                        if(StringUtils.isBlank(sellingPrice)){
                            //买进：1.0007200000000001
                            account.setPurchasePrice((String) xpath.evaluate("Cell[6]/Data", item,XPathConstants.STRING));
                        }else{
                            account.setSellingPrice(sellingPrice);
                        }
                        accountLinkedList.addLast(account);
                        cellBlock++;
                    }
                }
                rowInd++;
            }
        }
        //ENDDD//========================================================================================== foreach nodeList
        accountResponse.setAccountLinkedList(accountLinkedList);
        return accountResponse;
    }


}
