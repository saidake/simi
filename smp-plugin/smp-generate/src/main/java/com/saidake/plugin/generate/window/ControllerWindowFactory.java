package com.saidake.plugin.generate.window;

import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowFactory;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiManager;
import com.intellij.ui.content.Content;
import com.intellij.ui.content.ContentFactory;
import com.saidake.plugin.generate.data.vo.node.MethodNode;
import com.saidake.plugin.generate.data.vo.node.ControllerNode;
import com.saidake.plugin.generate.data.vo.node.RequestNode;
import com.saidake.plugin.generate.data.core.DataHolder;
import com.saidake.plugin.generate.data.regex.PatternHolder;
import com.saidake.plugin.generate.util.SmpControllerUtils;
import com.saidake.plugin.generate.util.SmpXmlUtils;
import lombok.extern.slf4j.Slf4j;
import org.dom4j.Document;
import org.dom4j.DocumentException;
import org.dom4j.io.SAXReader;
import org.jetbrains.annotations.NotNull;


import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;

/**
 * Controller window.
 *
 * @since   1.4
 * @author  Craig Brown
 */
@Slf4j
public class ControllerWindowFactory implements ToolWindowFactory {

    @Override
    public void createToolWindowContent(@NotNull Project project, @NotNull ToolWindow toolWindow) {
        Editor editor = FileEditorManager.getInstance(project).getSelectedTextEditor();
        if(editor!=null){
            VirtualFile file = FileDocumentManager.getInstance().getFile(editor.getDocument());
            int offset = editor.getCaretModel().getOffset();
            PsiElement element = PsiManager.getInstance(project).findFile(file).findElementAt(offset);
        }
        DataHolder.getInstance().getState().clearAll();
        //if(DataHolder.getInstance().getState().getPomProjectList().isEmpty())handleInitProjectData(project,project.getBaseDir().getChildren());
        handleInitProjectData(project,project.getBaseDir().getChildren());
        //A. build window
        ControllerWindow controllerAnchorWindow=new ControllerWindow(project,toolWindow);
        ContentFactory instance = ContentFactory.SERVICE.getInstance();
        Content content = instance.createContent(controllerAnchorWindow.getMainPanel(),"",false);
        toolWindow.getContentManager().addContent(content);
    }

    /**
     * Check pom file with VirtualFile。
     *
     * @param childrenList child file list
     */
    @SuppressWarnings("UnsafeVfsRecursion")
    private void handleInitProjectData(Project project, @NotNull VirtualFile[] childrenList) {
        boolean hasPom=false;
        //A. check whether pom.xml exists
        String currentProjectName=null;
        for (VirtualFile virtualFile : childrenList) {
            if("pom.xml".equals(virtualFile.getName())){
                hasPom=true;
                try {
                    SAXReader reader = new SAXReader();
                    Document pomDocument = reader.read(virtualFile.getInputStream());
                    currentProjectName = SmpXmlUtils.getXpathContent(pomDocument, "/project/artifactId");
                    List<String> moduleList = SmpXmlUtils.getXpathContentList(pomDocument, "/project/modules/module");
                    DataHolder.getInstance().getState().getPomProjectList().add(currentProjectName);
                    DataHolder.getInstance().getState().getPomProjectList().addAll(moduleList);
                    System.out.println("currentProjectName"+currentProjectName);
                } catch (DocumentException | IOException e) {
                    e.printStackTrace();
                }
            }
        }
        if(!hasPom)return;
        for (VirtualFile virtualFile : childrenList) {
            if(virtualFile.isDirectory()&&"src".equals(virtualFile.getName())){
                assert currentProjectName!=null;
                //B. src folder
                handleCheckSrcFolder(virtualFile,currentProjectName);
            } else if(virtualFile.isDirectory()){
                //B. check other folder(may also be a pom project)
                handleInitProjectData(project,virtualFile.getChildren());
            }
        }
    }

    /**
     * check src folder.
     *
     * @param virtualFile virtual file
     * @param currentProjectName   project name
     */
    private void handleCheckSrcFolder(VirtualFile virtualFile, String currentProjectName){
        VirtualFile fileByRelativePath = virtualFile.findFileByRelativePath("main/java");
        handleCheckControllerFileList(fileByRelativePath,currentProjectName);
    }

    private void handleCheckControllerFileList(VirtualFile virtualFile, String currentProjectName) {
        if(virtualFile==null)return;
        //A. foreach java folder files
        for (VirtualFile child : virtualFile.getChildren()) {
            //B. check Controller files in the current project.
            if (child.isDirectory()){
                handleCheckControllerFileList(child,currentProjectName);
            }else{
                try {
                    Map<String, List<ControllerNode>> projectControllerList = DataHolder.getInstance().getState().getProjectControllerList();
                    List<ControllerNode> controllerNodeList;
                    if(projectControllerList.containsKey(currentProjectName)){
                        controllerNodeList =projectControllerList.get(currentProjectName);
                    }else{
                        controllerNodeList =new ArrayList<>();
                        projectControllerList.put(currentProjectName, controllerNodeList);
                    }
                    byte[] bytes = child.contentsToByteArray();
                    String fileContent = new String(bytes, StandardCharsets.UTF_8);
                    //B. check whether it is a controller file.
                    if(fileContent.contains("@RestController")||fileContent.contains("@Controller")){
                        log.info("controller file: {}",child.getName());
                        controllerNodeList.add(handleControllerInfo(child, fileContent));
                    }
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }



    private ControllerNode handleControllerInfo(VirtualFile child, String fileContent) {
        Matcher classMatcher = PatternHolder.classPattern.matcher(fileContent);
        Matcher methodMatcher = PatternHolder.methodPattern.matcher(fileContent);
        Matcher requestMappingMatcher = PatternHolder.requestMappingPattern.matcher(fileContent);
        Matcher springDocTagMatcher = PatternHolder.springDocTagPattern.matcher(fileContent);
        Matcher springDocOperationMatcher = PatternHolder.springDocOperationPattern.matcher(fileContent);
        Matcher callMethodMatcher = PatternHolder.callMethodPattern.matcher(fileContent);

        ControllerNode controllerNode =new ControllerNode();
        String packagePath= SmpControllerUtils.getPackagePath(fileContent).orElseThrow();
        String className=null;
        //A. get class location and information.
        if(classMatcher.find()){
            int classLineIndex=classMatcher.start();
            className=classMatcher.group(3);
            //A. get the url of annotation @RequestMapping for the current class.
            if(requestMappingMatcher.find()){
                //B. match result url
                String currentUrlMatch=requestMappingMatcher.group(4)==null?requestMappingMatcher.group(6):requestMappingMatcher.group(4);
                //B. IF(classLineIndex) the url annotated on the current class.
                if(requestMappingMatcher.start()<classLineIndex){
                    controllerNode.setPrefixUrl(currentUrlMatch);
                    //B. ELSE IF(classLineIndex) class url，reset pointer.
                }else{
                    requestMappingMatcher.reset();
                }
            }
            //A. Tag controller title
            if(springDocTagMatcher.find()){
                controllerNode.setSpringDocTag(springDocTagMatcher.group(2));
            }
        }else{
            log.error("cannot find class");
        }
        controllerNode.setPackagePath(packagePath+"."+className);
        controllerNode.setFilePath(child.getPath());
        controllerNode.setFileName(child.getName());
        //A. get method name list to build RequestNode.
        log.info("======================================== controller method");
        String nextUrl=null;
        Integer nextRequestStart=null;
        String nextSummary=null;
        String nextDescription=null;
        Integer nextOperationStart=null;
        RequestNode requestNode =new RequestNode();
        List<RequestNode> requestNodeList =new ArrayList<>();
        // A. foreach methodMatcher
        int methodMatcherIndex=0;
        while (methodMatcher.find(methodMatcherIndex)){
            methodMatcherIndex=methodMatcher.end();
            String methodName = methodMatcher.group(4);
            log.info("fileName: "+child.getName());
            log.info("methodName: "+methodName);
            MethodNode requestNodeMethodNode = requestNode.getMethodNode();
            requestNodeMethodNode.setPackagePath(packagePath+"."+methodName);            //A. check title
            requestNodeMethodNode.setMethodName(methodName);
            //A. The current method has been checked once.
            if(nextSummary!=null&&nextOperationStart<methodMatcherIndex){
                requestNode.setSpringDocOperation(nextSummary);
                requestNode.setDescription(nextDescription);
                nextSummary=null;
                nextDescription=null;
                nextOperationStart=null;
            // A. check springDocOperationMatcher
            } else if(springDocOperationMatcher.find()){
                String checkSpringDocString = springDocOperationMatcher.group(1);
                String summary = checkSpringDocString.contains("summary")?
                        checkSpringDocString.replaceAll("^.*?summary\\s*?=\\s*?\"","").replaceAll("\".*$","")
                        :null;
                String description = checkSpringDocString.contains("description")?
                        checkSpringDocString.replaceAll("^.*?description\\s*?=\\s*?\"","").replaceAll("\".*$","")
                        :null;
                if(springDocOperationMatcher.start()<methodMatcherIndex){
                    requestNode.setSpringDocOperation(summary);
                    requestNode.setDescription(description);
                }else{
                    //B. The current method is not.
                    nextSummary=summary;
                    nextDescription=description;
                    nextOperationStart=springDocOperationMatcher.start();
                }
                //B. check callMethodMatcher
                int methodMatcherEnd = methodMatcher.end();
                Matcher regionCallMethodMatcher;
                if(methodMatcher.find(methodMatcher.end())){
                    regionCallMethodMatcher = callMethodMatcher.region(methodMatcherEnd, methodMatcher.start());
                }else{
                    regionCallMethodMatcher = callMethodMatcher.region(methodMatcherEnd,methodMatcher.regionEnd());
                }
                //B. check the method in detection interval: method start - next method start.
                while (regionCallMethodMatcher.find()){
                    MethodNode sonMethodNode = MethodNode.fromMethodName(callMethodMatcher.group(5));
                    requestNodeMethodNode.getChildMethodNodeList().add(sonMethodNode);
                }
                methodMatcherIndex=methodMatcherEnd;
            }

            //A. The current method has been checked once.
            if(nextUrl!=null&&nextRequestStart<methodMatcherIndex) {
                requestNode.setUrl(nextUrl);
                requestNodeMethodNode.setPackagePath(packagePath);
                requestNodeMethodNode.setFilePath(child.getPath());
                requestNodeList.add(requestNode);
                requestNode =new RequestNode();
                nextUrl=null;
                nextRequestStart=null;
            } else if(requestMappingMatcher.find()){
                    String currentUrl = requestMappingMatcher.group(4) == null ? requestMappingMatcher.group(6) : requestMappingMatcher.group(4);
                    if(controllerNode.getPrefixUrl()!=null)currentUrl= controllerNode.getPrefixUrl()+currentUrl;
                    //B. Annotation @RequestMapping is in front of the current method
                    if(requestMappingMatcher.start()<methodMatcherIndex){
                        requestNode.setUrl(currentUrl);
                        requestNodeList.add(requestNode);
                        requestNode =new RequestNode();
                    //B. @RequestMapping is temporarily stored after method for the current nextRequestStart and nextUrl, so there is no need for requestMappingMatcher. find next time
                    }else{
                        //B. The current method is not.
                        nextUrl=currentUrl;
                        nextRequestStart=requestMappingMatcher.start();
                    }
                }
            }
        controllerNode.setRequestNodeList(requestNodeList);
        return controllerNode;
    }

}
