package com.simi.service.trade.service.sax;

import com.alibaba.excel.util.StringUtils;
import com.simi.service.trade.domain.FxcmAccount;
import com.simi.service.trade.domain.FxcmAccountResponse;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

public class FxcmAccountParser extends DefaultHandler {
    private final String DATA_TAG_NAME="ss:Data";

    private final String DATA_TAG_START_CHECK="Created By";
    private final String TOTAL_TAG_START_CHECK="Total:";  // if this content appears, accountDataStart=false

    private boolean accountDataStart=false;
    private boolean firstAccountDataStarted=false;  // skip title
    private boolean dataTagStart=false;
    private int columnIndex=0;
    private int rowIndex=0;
    private FxcmAccount currentFxcmAccount=new FxcmAccount();

    FxcmAccountResponse fxcmAccountResponse=new FxcmAccountResponse();

    @Override
    public void startElement (String uri, String localName,
                              String qName, Attributes attributes)
            throws SAXException
    {
        if(this.DATA_TAG_NAME.equals(qName)) this.dataTagStart=true;

    }

    @Override
    public void endElement (String uri, String localName, String qName)
            throws SAXException {
        if (this.DATA_TAG_NAME.equals(qName)) this.dataTagStart = false;
    }

    @Override
    public void characters (char ch[], int start, int length)
            throws SAXException
    {
        String content= new String(ch, start, length);
        if(this.accountDataStart&&content.startsWith(TOTAL_TAG_START_CHECK))this.accountDataStart=false;
        if(this.accountDataStart&&dataTagStart){
            if(this.columnIndex>=14){
                this.columnIndex=0;
                this.rowIndex++;
                if(this.rowIndex%2==0){ // odd row
                    this.currentFxcmAccount= new FxcmAccount();
                    fxcmAccountResponse.getFxcmAccountList().add(this.currentFxcmAccount);
                }
            }
            this.setFxcmAccountColumnValue(content,this.columnIndex,this.rowIndex, this.currentFxcmAccount);
            this.columnIndex++;
        }
        if(!this.firstAccountDataStarted&&content.startsWith(DATA_TAG_START_CHECK)){
            this.accountDataStart=true;this.firstAccountDataStarted=true;
        }
    }


    @Override
    public void endDocument ()
            throws SAXException
    {
        System.out.println(this.fxcmAccountResponse);
    }


    /**
     * Sets the field values of {@link FxcmAccount} according to the index.
     *
     * @param content
     * @param index
     * @param fxcmAccount
     */
    private void setFxcmAccountColumnValue(String content, int index, int rowIndex, FxcmAccount fxcmAccount){
        if(rowIndex%2!=0&& StringUtils.isBlank(content))return;
        switch(index){
            case 0:fxcmAccount.setTicket(content);break;
            case 1:fxcmAccount.setSymbol(content);break;
            case 2:fxcmAccount.setVolume(content);break;
            case 3:
                if(rowIndex%2==0) fxcmAccount.setOpenDate(content);
                else fxcmAccount.setCloseDate(content);
                break;
            case 4:fxcmAccount.setSold(content);break;
            case 5:
                fxcmAccount.setBought(content);
                if(rowIndex%2==0) fxcmAccount.setBuy(true);
                break;
            case 6:fxcmAccount.setGrossPL(content);break;
            case 7:fxcmAccount.setComm(content);break;
            case 8:fxcmAccount.setDividends(content);break;
            case 9:fxcmAccount.setRollover(content);break;
            case 10:fxcmAccount.setAdj(content);break;
            case 11:fxcmAccount.setNetPL(content);break;
            case 12:
                if(rowIndex%2==0) fxcmAccount.setOpenCondition(content);
                else fxcmAccount.setCloseCondition(content);
                break;
            case 13:fxcmAccount.setCreatedBy(content);break;
        }
    }
}
