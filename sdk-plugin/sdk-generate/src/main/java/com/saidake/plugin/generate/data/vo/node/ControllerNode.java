package com.saidake.plugin.generate.data.vo.node;

import com.saidake.plugin.generate.data.vo.core.SmpTreeNode;

import java.util.ArrayList;
import java.util.List;

/**
 * Controller信息
 */
public class ControllerNode extends SmpTreeNode {
    /**
     * doc 标题
     */
    private String title;

    /**
     * controller的 前缀 网址
     */
    private String prefixUrl;

    private List<RequestNode> requestNodeList =new ArrayList<>();

    public ControllerNode(String packagePath, String filePath) {
        super(packagePath, filePath);
    }

    public ControllerNode() {
        super();
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public List<RequestNode> getRequestNodeList() {
        return requestNodeList;
    }

    public void setRequestNodeList(List<RequestNode> requestNodeList) {
        this.requestNodeList = requestNodeList;
    }

    public String getPrefixUrl() {
        return prefixUrl;
    }

    public void setPrefixUrl(String prefixUrl) {
        this.prefixUrl = prefixUrl;
    }
}
