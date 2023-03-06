package com.saidake.plugin.generate.data.core;




import com.saidake.plugin.generate.data.vo.node.ControllerNode;
import com.saidake.plugin.generate.data.vo.node.MethodNode;
import com.saidake.plugin.generate.data.vo.node.RequestNode;

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
    private Set<String> pomProjectList=new TreeSet<>();

    /**
     * pom项目内部Controller结构. [projectName - ControllerNode]
     */
    private Map<String, List<ControllerNode>> projectControllerList=new HashMap<>();
    private Map<String, ControllerNode> controllerNodeMap=new HashMap<>();
    private Map<String, RequestNode> requestNodeMap=new HashMap<>();
    private Map<String, MethodNode> methodNodeMap=new HashMap<>();
    //======================================================================= data tool
    public Set<String> getPomProjectList() {
        return pomProjectList;
    }

    public void setPomProjectList(Set<String> pomProjectList) {
        this.pomProjectList = pomProjectList;
    }

    public void setProjectControllerList(Map<String, List<ControllerNode>> projectControllerList) {
        this.projectControllerList = projectControllerList;
    }

    public Map<String, ControllerNode> getControllerNodeMap() {
        return controllerNodeMap;
    }

    public void setControllerNodeMap(Map<String, ControllerNode> controllerNodeMap) {
        this.controllerNodeMap = controllerNodeMap;
    }

    public Map<String, RequestNode> getRequestNodeMap() {
        return requestNodeMap;
    }

    public void setRequestNodeMap(Map<String, RequestNode> requestNodeMap) {
        this.requestNodeMap = requestNodeMap;
    }

    public Map<String, MethodNode> getMethodNodeMap() {
        return methodNodeMap;
    }

    public void setMethodNodeMap(Map<String, MethodNode> methodNodeMap) {
        this.methodNodeMap = methodNodeMap;
    }

    public Map<String, List<ControllerNode>> getProjectControllerList() {
        return projectControllerList;
    }

    @Override
    public String toString() {
        return "DataState{" +
                "pomProjectList=" + pomProjectList +
                '}';
    }
}
