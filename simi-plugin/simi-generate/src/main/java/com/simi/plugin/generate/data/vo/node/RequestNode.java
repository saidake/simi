package com.simi.plugin.generate.data.vo.node;

import com.simi.plugin.generate.data.vo.core.SmpTreeNode;
import lombok.Data;
import lombok.EqualsAndHashCode;


@EqualsAndHashCode(callSuper = true)
@Data
public class RequestNode extends SmpTreeNode {
    private String springDocOperation;
    private String description;
    private String url;
    private MethodNode methodNode =new MethodNode();
}
