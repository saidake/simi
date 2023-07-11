package com.simi.trade.controller.foreignExchange;

import com.simi.trade.domain.fuhui.Account;
import com.simi.trade.domain.fuhui.AccountResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import java.io.*;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@RestController
@Tag(name = "FuHui",description = "福汇交易平台")
@Slf4j
@RequestMapping("fuhui")
public class FuHuiController {

    @Value("${fuhui.account-info-path}")
    private String fuhuiDirectory;

    @GetMapping("/account")
    @Operation(description = "获取账号交易历史信息")
    public ResponseEntity<AccountResponse> getAccountInfo() throws IOException, SAXException, ParserConfigurationException, XPathExpressionException {
        // remove error char
        File file = new File(fuhuiDirectory);
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
        return ResponseEntity.ok(accountResponse);
    }
}
