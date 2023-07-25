package com.simi.service.trade.domain.file;

public class SdkSheet {
    protected MidSheet sheet;
    public String getSheetName() {
        return this.sheet.getName();
    }
}
