package com.saidake.plugin.generate.data.vo.tree;



public class UserTreeNode {
    private String content;
    private String filePath;

    public String getContent() {
        return content;
    }

    public void setContent(String content) {
        this.content = content;
    }

    public String getFilePath() {
        return filePath;
    }

    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }

    @Override
    public String toString() {
        return content;
    }
}
