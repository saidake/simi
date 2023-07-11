package com.simi.plugin.generate.data.vo.node;

import com.simi.plugin.generate.data.vo.core.SmpTreeNode;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
public class MethodNode extends SmpTreeNode {
    private String methodName;
    private List<MethodNode> childMethodNodeList=new ArrayList<>();

    public static MethodNode fromMethodName(String methodName){
        MethodNode methodNode = new MethodNode();
        methodNode.setMethodName(methodName);
        return methodNode;
    }
}
