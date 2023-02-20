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
import com.saidake.plugin.generate.data.reflect.MethodInfo;
import com.saidake.plugin.generate.data.request.ControllerInfo;
import com.saidake.plugin.generate.data.request.RequestInfo;
import com.saidake.plugin.generate.data.DataHolder;
import com.saidake.plugin.generate.data.regex.PatternHolder;
import com.saidake.plugin.generate.xml.PomParseHandler;
import org.jetbrains.annotations.NotNull;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import java.util.regex.Matcher;

/**
 * A factory object that generates ControllerWindow objects.
 *
 * @author  Zou, Hao David
 * @see     ControllerWindow
 * @since   1.4
 */
public class ControllerWindowFactory implements ToolWindowFactory {
    private Logger logger=Logger.getLogger(ControllerWindowFactory.class.getName());

    public void init(ToolWindow window) {
    }

    /**
     * Set properties of toolWindow object.
     *
     * @param project
     * @param toolWindow
     */
    @Override
    public void createToolWindowContent(@NotNull Project project, @NotNull ToolWindow toolWindow) {
        Editor editor = FileEditorManager.getInstance(project).getSelectedTextEditor();
        if(editor!=null){
            VirtualFile file = FileDocumentManager.getInstance().getFile(editor.getDocument());
            int offset = editor.getCaretModel().getOffset();
            PsiElement element = PsiManager.getInstance(project).findFile(file).findElementAt(offset);
        }
        //A. 获取基础数据，检查pom结构和controller结构
        handleInitBaseData(project,project.getBaseDir().getChildren());
        //A. 构建window
        ControllerWindow controllerAnchorWindow=new ControllerWindow(project,toolWindow);
        ContentFactory instance = ContentFactory.SERVICE.getInstance();
        Content content = instance.createContent(controllerAnchorWindow.getMainPanel(),"",false);
        toolWindow.getContentManager().addContent(content);
    }

    /**
     * Check pom file with {@link VirtualFile}.
     *
     * @param childrenList children file list with {@link VirtualFile}.
     */
    @SuppressWarnings("UnsafeVfsRecursion")
    private void handleInitBaseData(Project project, @NotNull VirtualFile[] childrenList) {
        boolean hasPom=false;
        PomParseHandler pomParseHandler=new PomParseHandler();
        for (VirtualFile virtualFile : childrenList) {
            if("pom.xml".equals(virtualFile.getName())){
                hasPom=true;
                try {
                    XMLReader xmlReader = XMLReaderFactory.createXMLReader();
                    xmlReader.setContentHandler(pomParseHandler);
                    xmlReader.setErrorHandler(pomParseHandler);
                    xmlReader.parse(new InputSource(virtualFile.getInputStream()));
                } catch (SAXException | IOException e) {
                    e.printStackTrace();
                }
            }
        }
        if(!hasPom)return;
        for (VirtualFile virtualFile : childrenList) {
            if(virtualFile.isDirectory()&&"src".equals(virtualFile.getName())){
                String currentProjectName = pomParseHandler.getCurrentProjectName();
                checkSrcFolder(virtualFile,currentProjectName);
            } else if(virtualFile.isDirectory()){
                handleInitBaseData(project,virtualFile.getChildren());
            }
        }
    }

    private void checkSrcFolder(VirtualFile virtualFile, String currentProjectName){
        VirtualFile fileByRelativePath = virtualFile.findFileByRelativePath("main/java");
        checkControllerInControllerFile(fileByRelativePath,currentProjectName);
    }

    private void checkControllerInControllerFile(VirtualFile virtualFile, String currentProjectName) {
        if(virtualFile==null)return;
        //A. 检查
        for (VirtualFile child : virtualFile.getChildren()) {
            if (child.isDirectory()){
                checkControllerInControllerFile(child,currentProjectName);
            }else{
                try {
                    //B. 会将获取的controller信息存储到DataHolder.getInstance().getState().getProjectControllerList()
                    Map<String, List<ControllerInfo>> projectControllerList = DataHolder.getInstance().getState().getProjectControllerList();
                    List<ControllerInfo> controllerInfoList;
                    if(projectControllerList.containsKey(currentProjectName)){
                        controllerInfoList=projectControllerList.get(currentProjectName);
                    }else{
                        controllerInfoList=new ArrayList<>();
                        projectControllerList.put(currentProjectName,controllerInfoList);
                    }
                    byte[] bytes = child.contentsToByteArray();
                    String fileContent = new String(bytes, StandardCharsets.UTF_8);
                    //B. 检查是不是Controller文件，并添加Pattern判断
                    if(fileContent.contains("@RestController")||fileContent.contains("@Controller")){
                        controllerInfoList.add(handleControllerInfo(child, fileContent));
                    }
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }

    }




    private ControllerInfo handleControllerInfo(VirtualFile child, String fileContent) {
        Matcher packageMatcher= PatternHolder.packagePattern.matcher(fileContent);
        Matcher classMatcher = PatternHolder.classPattern.matcher(fileContent);
        Matcher methodMatcher = PatternHolder.methodPattern.matcher(fileContent);
        Matcher requestMappingMatcher = PatternHolder.requestMappingPattern.matcher(fileContent);
        Matcher springDocTagMatcher = PatternHolder.springDocTagPattern.matcher(fileContent);
        Matcher springDocOperationMatcher = PatternHolder.springDocOperationPattern.matcher(fileContent);
        ControllerInfo controllerInfo=new ControllerInfo();
        String packagePath=null;
        String className=null;
        System.out.println("controllerfile: "+child.getName());
        //A. 获取package 位置
        if(packageMatcher.find()){
            packagePath=packageMatcher.group(1);
        }else{
            logger.severe("cannot find class");
        }
        assert packagePath!=null;
        //A. 获取class 位置 和class 信息
        if(classMatcher.find()){
            int classLineIndex=classMatcher.start();
            className=classMatcher.group(3);
            //A. 获取class的@RequestMapping的url
            if(requestMappingMatcher.find()){
                //B. 匹配到的 url
                String currentUrlMatch=requestMappingMatcher.group(4)==null?requestMappingMatcher.group(6):requestMappingMatcher.group(4);
                //B. IF(classLineIndex) class上方标注的 url
                if(requestMappingMatcher.start()<classLineIndex){
                    controllerInfo.setHeaderUrl(currentUrlMatch);
                    //B. ELSE IF(classLineIndex) class内部 url，指针恢复
                }else{
                    requestMappingMatcher.reset();
                }
            }
            //A. 获取Tag controller title
            if(springDocTagMatcher.find()){
                controllerInfo.setTitle(springDocTagMatcher.group(2));
            }
        }else{
            logger.severe("cannot find class");
        }
        controllerInfo.setPackagePath(packagePath+"."+className);
        controllerInfo.setFilePath(child.getPath());


        //A. 获取method方法名 List
        System.out.println("========================================test method");
        String nextUrl=null;
        Integer nextRequestStart=null;
        String nextSummary=null;
        String nextDescription=null;
        Integer nextOperationStart=null;
        RequestInfo requestInfo=new RequestInfo();
        List<RequestInfo> requestInfoList=new ArrayList<>();
        while (methodMatcher.find()){
            System.out.println("fileName: "+child.getName());
            String methodName = methodMatcher.group(4);
            System.out.println("methodName: "+methodName);
            requestInfo.getMethodInfo().setPackagePath(packagePath+"."+methodName);            //A. 检查 title

            //A. 已经检查过一次了
            if(nextSummary!=null&&nextOperationStart<methodMatcher.start()){
                requestInfo.setTitle(nextSummary);
                requestInfo.setDescription(nextDescription);
                nextSummary=null;
                nextDescription=null;
                nextOperationStart=null;
            } else if(springDocOperationMatcher.find()){
                String checkSpringDocString = springDocOperationMatcher.group(1);
                String summary = checkSpringDocString.contains("summary")?
                        checkSpringDocString.replaceAll("^.*?summary\\s*?=\\s*?\"","").replaceAll("\".*$","")
                        :null;
                String description = checkSpringDocString.contains("description")?
                        checkSpringDocString.replaceAll("^.*?description\\s*?=\\s*?\"","").replaceAll("\".*$","")
                        :null;
                if(springDocOperationMatcher.start()<methodMatcher.start()){
                    requestInfo.setTitle(summary);
                    requestInfo.setDescription(description);
                }else{
                    //B. 当前方法不是
                    nextSummary=summary;
                    nextDescription=description;
                    nextOperationStart=springDocOperationMatcher.start();
                }
            }

            //A. IF(nextUrl) 已经检查过一次了
            if(nextUrl!=null&&nextRequestStart<methodMatcher.start()) {
                requestInfo.setUrl(nextUrl);
                requestInfo.setMethodInfo(new MethodInfo(packagePath,child.getPath()));
                requestInfoList.add(requestInfo);
                requestInfo=new RequestInfo();
                nextUrl=null;
                nextRequestStart=null;
            //A. ELSE IF:   [CORE]检查 url
            }else if(requestMappingMatcher.find()){
                    String currentUrl = requestMappingMatcher.group(4) == null ? requestMappingMatcher.group(6) : requestMappingMatcher.group(4);
                    if(controllerInfo.getHeaderUrl()!=null)currentUrl=controllerInfo.getHeaderUrl()+currentUrl;
                    if(requestMappingMatcher.start()<methodMatcher.start()){
                        requestInfo.setUrl(currentUrl);
                        requestInfoList.add(requestInfo);
                        requestInfo=new RequestInfo();
                    }else{
                        //B. 当前方法不是
                        nextUrl=currentUrl;
                        nextRequestStart=requestMappingMatcher.start();
                    }
                }
            }
        controllerInfo.setRequestInfoList(requestInfoList);
        return controllerInfo;
    }

}
