package com.saidake.plugin.generate.xml;

import com.saidake.plugin.generate.data.core.DataHolder;
import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;

import java.util.*;


public class PomParseHandler extends DefaultHandler {
    private boolean modulesStarted =false;  // 检测当前项目内的子项目列表 <modules>
    private boolean moduleStarted =false;  // 检测当前项目内的子项目 <module>
    private boolean artifactIdStarted =false; //检测当前项目的名称 <artifactId>
    private String currentProjectName; //检测当前项目的名称 <artifactId>，并对外 ControllerWindowFactory 提供当前处理的项目名称
    /**
     * 排除不需要检测的标签，并且这些标签的内部也不会被检测。
     */
    private final Map<String,Integer> artifactIdExcludeStartElementMap=new HashMap<String,Integer>(){{
        put("parent",0);
        put("dependencies",0);
        put("dependencyManagement",0);
        put("build",0);
    }};

    @Override
    public void startElement (String uri, String localName,
                              String qName, Attributes attributes)
    {
        if ("modules".equals(localName)){
            modulesStarted =true;
        }else if(modulesStarted &&"module".equals(localName)){
            moduleStarted =true;
        }else if(artifactIdExcludeStartElementMap.values().stream().noneMatch(item->item>0) &&"artifactId".equals(localName)){
            artifactIdStarted =true;
        }else if(artifactIdExcludeStartElementMap.containsKey(localName)){
            artifactIdExcludeStartElementMap.put(localName,artifactIdExcludeStartElementMap.get(localName)+1);
        }
    }

    @SuppressWarnings({"ConstantConditions", "unchecked"})
    @Override
    public void endElement (String uri, String localName, String qName){
        if ("modules".equals(localName)){
            modulesStarted =false;
        }else if(modulesStarted &&"module".equals(localName)){
            moduleStarted =false;
        }else if(artifactIdExcludeStartElementMap.values().stream().noneMatch(item->item>0)&&"artifactId".equals(localName)){
            //A. 检查到当前项目的名称 artifactId
            artifactIdStarted =false;
        }else if(artifactIdExcludeStartElementMap.containsKey(localName)){
            artifactIdExcludeStartElementMap.put(localName,artifactIdExcludeStartElementMap.get(localName)-1);
        }
    }

    @SuppressWarnings({"unchecked", "CStyleArrayDeclaration"})
    @Override
    public void characters (char ch[], int start, int length){
        Set<String> pomProjectList = DataHolder.getInstance().getState().getPomProjectList();
        //A. IF(moduleStart) 检查到符合要求的module
        if(moduleStarted){
            pomProjectList.add(
                    new String(Arrays.copyOfRange(ch, start, start+length))
            );
        //A. ELSE IF(artifactIdStart) 检查到当前项目名 artifactId
        }else if(artifactIdStarted){
            currentProjectName=new String(Arrays.copyOfRange(ch, start, start + length));
            pomProjectList.add(currentProjectName);
        }
    }

    public String getCurrentProjectName() {
        return currentProjectName;
    }

    public void setCurrentProjectName(String currentProjectName) {
        this.currentProjectName = currentProjectName;
    }
}
