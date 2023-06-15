package com.saidake.plugin.init.core.support.smyml;

import lombok.Data;

import java.util.List;

@Data
public class WriteInfo {
    private String write;
    private String read;
    private String type;
    private String backup=BackupEnum.CURRENT.getValue();
    private Boolean once;
    private List<String> rpRuleList;
}
