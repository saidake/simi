package com.simi.plugin.generate.data.vo.core;

import com.simi.plugin.generate.data.vo.node.ControllerNode;
import com.simi.plugin.generate.data.vo.node.MethodNode;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * File info
 *
 * @see ControllerNode
 * @see MethodNode
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public abstract class SmpTreeNode {
    private String packagePath;
    private String filePath;
    private String fileName;
}
