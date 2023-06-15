package com.saidake.common.core.util.file.support.yaml;

import lombok.Data;

import java.util.List;

@Data
public class ProjectInfo {
    private String name;
    private String path;
    private String env;
    private List<WriteInfo> ruleList;
}
