package com.saidake.plugin.generate.data;




import com.saidake.plugin.generate.data.request.ControllerInfo;

import java.util.*;

public class DataState {
    public DataState(){
    }

    /**
     * pom 项目级联结构
     */
    private Set<String> pomProjectList=new TreeSet<>();


    /**
     * pom项目内部Controller结构
     */
    private Map<String, List<ControllerInfo>> projectControllerList=new HashMap<>();


    //======================================================================= data tool
    public Set<String> getPomProjectList() {
        return pomProjectList;
    }

    public void setPomProjectList(Set<String> pomProjectList) {
        this.pomProjectList = pomProjectList;
    }

    public Map<String, List<ControllerInfo>> getProjectControllerList() {
        return projectControllerList;
    }

    @Override
    public String toString() {
        return "DataState{" +
                "pomProjectList=" + pomProjectList +
                '}';
    }
}
