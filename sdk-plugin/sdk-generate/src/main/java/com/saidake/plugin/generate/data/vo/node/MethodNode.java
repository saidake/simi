package com.saidake.plugin.generate.data.vo.node;

import com.saidake.plugin.generate.data.vo.core.SmpTreeNode;

import java.util.List;

public class MethodNode extends SmpTreeNode {

    private List<MethodNode> childMethodNodeList;

    public MethodNode(String packagePath, String filePath) {
        super(packagePath, filePath);
    }
    public MethodNode() {
        super();
    }
}
