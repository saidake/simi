package com.saidake.plugin.generate.xml;

import com.saidake.plugin.generate.data.core.DataHolder;
import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;

import java.util.*;

public class PomParseHandler extends DefaultHandler {
    private boolean modulesStart=false;
    private boolean moduleStart=false;
    private boolean artifactIdStart=false; //检测当前项目的名称 artifactId
    private String currentProjectName; //检测当前项目的名称 artifactId
    private Map<String,Integer> artifactIdExcludeStartElementMap=new HashMap<String,Integer>(){{
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
            modulesStart=true;
        }else if(modulesStart&&"module".equals(localName)){
            moduleStart=true;
        }else if(artifactIdExcludeStartElementMap.values().stream().noneMatch(item->item>0) &&"artifactId".equals(localName)){
            artifactIdStart=true;
        }else if(artifactIdExcludeStartElementMap.containsKey(localName)){
            artifactIdExcludeStartElementMap.put(localName,artifactIdExcludeStartElementMap.get(localName)+1);
        }
    }

    @SuppressWarnings({"ConstantConditions", "unchecked"})
    @Override
    public void endElement (String uri, String localName, String qName){
        if ("modules".equals(localName)){
            modulesStart=false;
        }else if(modulesStart&&"module".equals(localName)){
            moduleStart=false;
        }else if(artifactIdExcludeStartElementMap.values().stream().noneMatch(item->item>0)&&"artifactId".equals(localName)){
            //A. 检查到当前项目的名称 artifactId
            artifactIdStart=false;
        }else if(artifactIdExcludeStartElementMap.containsKey(localName)){
            artifactIdExcludeStartElementMap.put(localName,artifactIdExcludeStartElementMap.get(localName)-1);
        }
    }

    @SuppressWarnings({"unchecked", "CStyleArrayDeclaration"})
    @Override
    public void characters (char ch[], int start, int length){
        Set<String> pomProjectList = DataHolder.getInstance().getState().getPomProjectList();
        //A. IF(moduleStart) 检查到符合要求的module
        if(moduleStart){
            pomProjectList.add(new String(Arrays.copyOfRange(ch, start, start+length)));
        //A. ELSE IF(artifactIdStart) 检查到当前项目名 artifactId
        }else if(artifactIdStart){
            currentProjectName = new String(Arrays.copyOfRange(ch, start, start + length));
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
