package com.saidake.plugin.generate.data.vo.core;

import com.saidake.plugin.generate.data.vo.node.ControllerNode;
import com.saidake.plugin.generate.data.vo.node.MethodNode;

/**
 * 文件信息
 *
 * @see ControllerNode
 * @see MethodNode
 */
public abstract class SmpTreeNode {
    private String packagePath;
    private String filePath;

    public String getPackagePath() {
        return packagePath;
    }
    public String getFilePath() {
        return filePath;
    }

    public void setPackagePath(String packagePath) {
        this.packagePath = packagePath;
    }

    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }

    public SmpTreeNode(String packagePath, String filePath) {
        this.packagePath = packagePath;
        this.filePath = filePath;
    }

    public SmpTreeNode() {
    }
}
