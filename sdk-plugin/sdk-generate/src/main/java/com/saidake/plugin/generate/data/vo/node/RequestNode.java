package com.saidake.plugin.generate.data.vo.node;

import com.saidake.plugin.generate.data.vo.core.SmpTreeNode;

/**
 * 请求信息，隶属于ControllerInfo
 */
public class RequestNode extends SmpTreeNode {
    private String title;
    private String description;
    private String url;
    private MethodNode methodNode =new MethodNode();

    public RequestNode(String url) {
        this.url = url;
    }
    public RequestNode( ) {
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }


    public MethodNode getMethodNode() {
        return methodNode;
    }

    public void setMethodNode(MethodNode methodNode) {
        this.methodNode = methodNode;
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
