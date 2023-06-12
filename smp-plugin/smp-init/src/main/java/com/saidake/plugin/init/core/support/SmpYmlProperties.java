package com.saidake.plugin.init.core.support;

import com.saidake.plugin.init.core.support.smyml.ProjectInfo;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class SmpYmlProperties {
    private List<ProjectInfo> project;
}
