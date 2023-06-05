package com.saidake.common.core.util.file.support;

import com.saidake.common.core.util.file.support.smyml.ProjectInfo;
import com.saidake.common.core.util.file.support.smyml.WriteInfo;
import lombok.Data;

import java.util.List;

@Data
public class SmpYmlProperties {
    private List<ProjectInfo> project;
}
