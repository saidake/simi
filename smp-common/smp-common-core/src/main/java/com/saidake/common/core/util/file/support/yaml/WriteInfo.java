package com.saidake.common.core.util.file.support.yaml;

import lombok.Data;

@Data
public class WriteInfo {
    private String write;
    private String read;
    private String type;
    private String backup;
    private Boolean once;
}
