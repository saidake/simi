package com.saidake.plugin.generate.data.core;




import com.saidake.plugin.generate.data.vo.node.ControllerNode;
import com.saidake.plugin.generate.data.vo.node.MethodNode;
import com.saidake.plugin.generate.data.vo.node.RequestNode;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.junit.Assert;

import java.util.*;

/**
 * 数据仓库
 */
public class DataState {
    DataState(){
    }
    /**
     * pom 项目级联结构
     */
    private Set<String> pomProjectList =new TreeSet<>();

    /**
     * pom项目内部 Controller 结构. [ projectName - List<ControllerNode>]
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
    public Set<String> getPomProjectList() {
        return pomProjectList;
    }

    public void setPomProjectList(Set<String> pomProjectList) {
        assert pomProjectList!=null;
        this.pomProjectList = pomProjectList;
    }

    public void setProjectControllerList(Map<String, List<ControllerNode>> projectControllerList) {
        assert projectControllerList!=null;
        this.projectControllerList = projectControllerList;
    }

    public Map<String, ControllerNode> getControllerNodeMap() {
        return controllerNodeMap;
    }

    public void setControllerNodeMap(Map<String, ControllerNode> controllerNodeMap) {
        assert controllerNodeMap!=null;
        this.controllerNodeMap = controllerNodeMap;
    }

    public Map<String, RequestNode> getRequestNodeMap() {
        return requestNodeMap;
    }

    public void setRequestNodeMap(Map<String, RequestNode> requestNodeMap) {
        assert requestNodeMap!=null;
        this.requestNodeMap = requestNodeMap;
    }

    public Map<String, MethodNode> getMethodNodeMap() {
        return methodNodeMap;
    }

    public void setMethodNodeMap(Map<String, MethodNode> methodNodeMap) {
        assert methodNodeMap!=null;
        this.methodNodeMap = methodNodeMap;
    }

    public Map<String, List<ControllerNode>> getProjectControllerList() {
        return projectControllerList;
    }


}
