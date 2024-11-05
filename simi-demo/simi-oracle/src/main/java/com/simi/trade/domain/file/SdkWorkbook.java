package com.simi.trade.domain.file;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.List;

public class SdkWorkbook {
    private List<SdkSheet> sheetList;

    public SdkWorkbook(InputStream inputStream) throws IOException {
        InputStream checkedStream = (inputStream.markSupported() ? inputStream : new BufferedInputStream(inputStream));
        HashMap context = new HashMap();
        context.clear();
    }

    public SdkSheet getSheet(String name) {
        return null;
//        return sheetList.stream().filter(sdkSheet -> sdkSheet.getSheetName().equalsIgnoreCase(name)).findAny().orElse(null);
    }
}
