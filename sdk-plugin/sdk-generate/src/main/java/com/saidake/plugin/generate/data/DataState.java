package com.saidake.plugin.generate.data;




import com.twelvemonkeys.util.LinkedSet;

import javax.annotation.Nullable;
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
    private Map<String, Map<String,String>> projectControllerList=new HashMap<>();


    //======================================================================= data tool
    @Nullable
    public Set<String> getPomProjectList() {
        return pomProjectList;
    }

    public void setPomProjectList(Set<String> pomProjectList) {
        this.pomProjectList = pomProjectList;
    }

    public Map<String, Map<String, String>> getProjectControllerList() {
        return projectControllerList;
    }

    @Override
    public String toString() {
        return "DataState{" +
                "pomProjectList=" + pomProjectList +
                '}';
    }
}
