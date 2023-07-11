package com.simi.plugin.generate.data.core;




import com.simi.plugin.generate.data.vo.node.ControllerNode;
import com.simi.plugin.generate.data.vo.node.MethodNode;
import com.simi.plugin.generate.data.vo.node.RequestNode;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.*;

@Data
@NoArgsConstructor
public class DataState {

    /**
     * Pom project module information.
     * Format: ["<project>", "<module>", "<module>"]
     */
    private Set<String> pomProjectList =new TreeSet<>();

    /**
     * Project controller information.
     * Format: { "<project>" : [ ControllerNode ] }
     */
    private Map<String, List<ControllerNode>> projectControllerList=new HashMap<>();
    private Map<String, ControllerNode> controllerNodeMap=new HashMap<>();
    private Map<String, RequestNode> requestNodeMap=new HashMap<>();
    private Map<String, MethodNode> methodNodeMap=new HashMap<>();
    //======================================================================= data tool
    public void clearAll(){
        pomProjectList.clear();
        clearControllerNodeList();
        clearAllTreeMapData();
    }
    public void clearControllerNodeList(){
        projectControllerList.clear();
    }
    public void clearAllTreeMapData(){
        controllerNodeMap.clear();
        requestNodeMap.clear();
        methodNodeMap.clear();
    }
    //======================================================================= getter setter
}
