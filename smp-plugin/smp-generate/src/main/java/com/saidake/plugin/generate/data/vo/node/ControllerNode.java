package com.saidake.plugin.generate.data.vo.node;

import com.saidake.plugin.generate.data.vo.core.SmpTreeNode;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.util.ArrayList;
import java.util.List;


@EqualsAndHashCode(callSuper = true)
@Data
public class ControllerNode extends SmpTreeNode {
    /**
     * doc title
     */
    private String springDocTag;

    /**
     * controller  prefix url
     */
    private String prefixUrl;

    private List<RequestNode> requestNodeList =new ArrayList<>();
}
