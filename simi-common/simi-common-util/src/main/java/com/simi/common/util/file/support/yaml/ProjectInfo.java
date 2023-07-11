package com.simi.common.util.file.support.yaml;

import lombok.Data;

import java.util.List;

@Data
public class ProjectInfo {
    private String name;
    private String path;
    private StringList envList;
    private String defaultEnv;
    private Boolean pomProjectNameCheck;
    private List<WriteInfo> ruleList;
}
