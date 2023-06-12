package com.saidake.plugin.init.core.support.smyml;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class WriteInfo {
    private String write;
    private String read;
    private String type;
    private String backup;
    private Boolean once;
}