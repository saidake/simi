package com.saidake.plugin.generate.data.vo.node;

import com.saidake.plugin.generate.data.vo.core.SmpTreeNode;

import java.util.ArrayList;
import java.util.List;

public class MethodNode extends SmpTreeNode {
    private String methodName;
    private List<MethodNode> childMethodNodeList=new ArrayList<>();

    public MethodNode(String packagePath, String filePath) {
        super(packagePath, filePath);
    }
    public MethodNode() {
        super();
    }
    public MethodNode(String methodName) {
        super();
        this.methodName=methodName;
    }

    public String getMethodName() {
        return methodName;
    }

    public void setMethodName(String methodName) {
        this.methodName = methodName;
    }

    public List<MethodNode> getChildMethodNodeList() {
        return childMethodNodeList;
    }

    public void setChildMethodNodeList(List<MethodNode> childMethodNodeList) {
        this.childMethodNodeList = childMethodNodeList;
    }
}
