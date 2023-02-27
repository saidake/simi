package com.saidake.plugin.generate.data.vo.reflect;

public class BaseInfo {
    private String packagePath;
    private String filePath;

    public String getPackagePath() {
        return packagePath;
    }

    public void setPackagePath(String packagePath) {
        this.packagePath = packagePath;
    }

    public String getFilePath() {
        return filePath;
    }

    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }

    public BaseInfo(String packagePath, String filePath) {
        this.packagePath = packagePath;
        this.filePath = filePath;
    }
}
