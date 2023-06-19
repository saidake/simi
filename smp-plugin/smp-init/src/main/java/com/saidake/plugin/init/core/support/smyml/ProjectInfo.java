package com.saidake.plugin.init.core.support.smyml;

import lombok.Data;

import java.util.List;

@Data
public class ProjectInfo {
    private String name;
    private String path;
    private List<String> envList;
    private String defaultEnv;
    private Boolean pomProjectNameCheck;
    private List<WriteInfo> ruleList;
}
