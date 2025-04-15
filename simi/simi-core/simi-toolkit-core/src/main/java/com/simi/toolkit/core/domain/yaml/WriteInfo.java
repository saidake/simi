package com.simi.toolkit.core.domain.yaml;

import com.simi.toolkit.core.domain.enums.BackupEnum;
import lombok.Data;

import java.util.List;

@Data
public class WriteInfo {
    private String write;
    private String read;
    private String type;
    private String backup= BackupEnum.CURRENT.getValue();
    private Boolean once;
    private List<String> rpRuleList;
    private StringList activeEnvList;
}
