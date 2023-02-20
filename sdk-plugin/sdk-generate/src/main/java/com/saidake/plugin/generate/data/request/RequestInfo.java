package com.saidake.plugin.generate.data.request;

import com.saidake.plugin.generate.data.reflect.MethodInfo;

/**
 * 请求信息
 */
public class RequestInfo {
    private String title;
    private String description;
    private String url;
    private MethodInfo methodInfo=new MethodInfo();

    public RequestInfo(String url) {
        this.url = url;
    }
    public RequestInfo( ) {
    }



    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }


    public MethodInfo getMethodInfo() {
        return methodInfo;
    }

    public void setMethodInfo(MethodInfo methodInfo) {
        this.methodInfo = methodInfo;
    }


    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }
}
