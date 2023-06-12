package com.saidake.plugin.init.core.support.smyml;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ProjectInfo {
    private String name;
    private String path;
    private String env;
    private List<WriteInfo> fileList;

}
