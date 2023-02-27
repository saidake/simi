package com.saidake.plugin.generate.data.vo.request;

import com.saidake.plugin.generate.data.vo.reflect.BaseInfo;

import java.util.ArrayList;
import java.util.List;

public class ControllerInfo extends BaseInfo {
    private String title;
    private String headerUrl;
    private List<RequestInfo> requestInfoList=new ArrayList<>();

    public ControllerInfo(String packagePath, String filePath) {
        super(packagePath, filePath);
    }

    public ControllerInfo() {
        super(null, null);
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public List<RequestInfo> getRequestInfoList() {
        return requestInfoList;
    }

    public void setRequestInfoList(List<RequestInfo> requestInfoList) {
        this.requestInfoList = requestInfoList;
    }

    public String getHeaderUrl() {
        return headerUrl;
    }

    public void setHeaderUrl(String headerUrl) {
        this.headerUrl = headerUrl;
    }
}
