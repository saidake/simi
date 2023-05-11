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
 * ControllerWindow 窗口对象工厂
 *
 * @author  Craig Hoffman
 * @see     ControllerWindowFactory
 * @since   1.4
 */
public class ControllerWindowFactory implements ToolWindowFactory {

    private final Logger logger=Logger.getLogger(ControllerWindowFactory.class.getName());

    public void init(ToolWindow window) {
    }

    /**
     * 设置窗口 ControllerWindow 的属性
     *
     * @param project 项目对象
     * @param toolWindow 窗口对象
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
        DataHolder.getInstance().getState().clearAll();
        //if(DataHolder.getInstance().getState().getPomProjectList().isEmpty())handleInitProjectData(project,project.getBaseDir().getChildren());
        handleInitProjectData(project,project.getBaseDir().getChildren());
        //A. 构建window
        ControllerWindow controllerAnchorWindow=new ControllerWindow(project,toolWindow);
        ContentFactory instance = ContentFactory.SERVICE.getInstance();
        Content content = instance.createContent(controllerAnchorWindow.getMainPanel(),"",false);
        toolWindow.getContentManager().addContent(content);
    }

    /**
     * 检查pom.xml文件，用{@link VirtualFile}。
     * 检查module项目结构
     *
     * @param childrenList 子文件列表 {@link VirtualFile}.
     */
    @SuppressWarnings("UnsafeVfsRecursion")
    private void handleInitProjectData(Project project, @NotNull VirtualFile[] childrenList) {
        boolean hasPom=false;
        //A. 检测pom.xml文件
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
                //B. 检查 src文件夹
                handleCheckSrcFolder(virtualFile,currentProjectName);
            } else if(virtualFile.isDirectory()){
                //B. 检查 普通文件夹，递归
                if(virtualFile.getPath().contains("pig-visual"))
                handleInitProjectData(project,virtualFile.getChildren());
                else continue;
            }
        }
    }

    /**
     * 检查src文件夹，检查Controller信息
     *
     * @param virtualFile 虚拟文件
     * @param currentProjectName  当前项目名
     */
    private void handleCheckSrcFolder(VirtualFile virtualFile, String currentProjectName){
        VirtualFile fileByRelativePath = virtualFile.findFileByRelativePath("main/java");
        handleCheckControllerFileList(fileByRelativePath,currentProjectName);
    }

    private void handleCheckControllerFileList(VirtualFile virtualFile, String currentProjectName) {
        if(virtualFile==null)return;
        //A. 检查
        for (VirtualFile child : virtualFile.getChildren()) {
            //B. 检查项目中的Controller文件
            if (child.isDirectory()){
                handleCheckControllerFileList(child,currentProjectName);
            }else{
                try {
                    //B. 会将获取的controller信息存储到DataHolder.getInstance().getState().getProjectControllerList()
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
                    //B. 检查是不是Controller文件，并添加Pattern判断
                    if(fileContent.contains("@RestController")||fileContent.contains("@Controller")){
                        logger.info("handle controller file: "+child.getName());
                        controllerNodeList.add(handleControllerInfo(child, fileContent));
                    }
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
    }



    private ControllerNode handleControllerInfo(VirtualFile child, String fileContent) {
        Matcher packageMatcher= PatternHolder.packagePattern.matcher(fileContent);
        Matcher classMatcher = PatternHolder.classPattern.matcher(fileContent);
        Matcher methodMatcher = PatternHolder.methodPattern.matcher(fileContent);
        Matcher requestMappingMatcher = PatternHolder.requestMappingPattern.matcher(fileContent);
        Matcher springDocTagMatcher = PatternHolder.springDocTagPattern.matcher(fileContent);
        Matcher springDocOperationMatcher = PatternHolder.springDocOperationPattern.matcher(fileContent);
        Matcher callMethodMatcher = PatternHolder.callMethodPattern.matcher(fileContent);

        ControllerNode controllerNode =new ControllerNode();
        String packagePath=null;
        String className=null;
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
                    controllerNode.setPrefixUrl(currentUrlMatch);
                    //B. ELSE IF(classLineIndex) class内部 url，指针恢复
                }else{
                    requestMappingMatcher.reset();
                }
            }
            //A. 获取Tag controller title
            if(springDocTagMatcher.find()){
                controllerNode.setSpringDocTag(springDocTagMatcher.group(2));
            }
        }else{
            logger.severe("cannot find class");
        }
        controllerNode.setPackagePath(packagePath+"."+className);
        controllerNode.setFilePath(child.getPath());
        controllerNode.setFileName(child.getName());
        //A. 获取method方法名 List，构建requestNode
        logger.info("======================================== controller method");
        String nextUrl=null;
        Integer nextRequestStart=null;
        String nextSummary=null;
        String nextDescription=null;
        Integer nextOperationStart=null;
        RequestNode requestNode =new RequestNode();
        List<RequestNode> requestNodeList =new ArrayList<>();
        // A. 循环检测 methodMatcher 匹配
        int methodMatcherIndex=0;
        while (methodMatcher.find(methodMatcherIndex)){
            methodMatcherIndex=methodMatcher.end();
            String methodName = methodMatcher.group(4);
            logger.info("fileName: "+child.getName());
            logger.info("methodName: "+methodName);
            MethodNode requestNodeMethodNode = requestNode.getMethodNode();
            requestNodeMethodNode.setPackagePath(packagePath+"."+methodName);            //A. 检查 title
            requestNodeMethodNode.setMethodName(methodName);
            //A. 已经检查过一次了
            if(nextSummary!=null&&nextOperationStart<methodMatcherIndex){
                requestNode.setSpringDocOperation(nextSummary);
                requestNode.setDescription(nextDescription);
                nextSummary=null;
                nextDescription=null;
                nextOperationStart=null;
            // A. 检测 springDocOperationMatcher 匹配
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
                    //B. 当前方法不是
                    nextSummary=summary;
                    nextDescription=description;
                    nextOperationStart=springDocOperationMatcher.start();
                }
                //B. 循环检测 callMethodMatcher 匹配
                int methodMatcherEnd = methodMatcher.end();
                Matcher regionCallMethodMatcher;
                if(methodMatcher.find(methodMatcher.end())){
                    regionCallMethodMatcher = callMethodMatcher.region(methodMatcherEnd, methodMatcher.start());
                }else{
                    regionCallMethodMatcher = callMethodMatcher.region(methodMatcherEnd,methodMatcher.regionEnd());
                }
                //B. 检测区间中的方法： method开始 - 下一个method开始
                while (regionCallMethodMatcher.find()){
                    MethodNode sonMethodNode = new MethodNode(callMethodMatcher.group(5));
                    requestNodeMethodNode.getChildMethodNodeList().add(sonMethodNode);
                }
                methodMatcherIndex=methodMatcherEnd;
            }

            //A. IF(nextUrl) 已经检查过一次了
            if(nextUrl!=null&&nextRequestStart<methodMatcherIndex) {
                requestNode.setUrl(nextUrl);
                requestNodeMethodNode.setPackagePath(packagePath);
                requestNodeMethodNode.setFilePath(child.getPath());
                requestNodeList.add(requestNode);
                requestNode =new RequestNode();
                nextUrl=null;
                nextRequestStart=null;
            //A. ELSE IF:   [CORE]检查 url
            } else if(requestMappingMatcher.find()){
                    String currentUrl = requestMappingMatcher.group(4) == null ? requestMappingMatcher.group(6) : requestMappingMatcher.group(4);
                    if(controllerNode.getPrefixUrl()!=null)currentUrl= controllerNode.getPrefixUrl()+currentUrl;
                    //B. @RequestMapping 在 method前面
                    if(requestMappingMatcher.start()<methodMatcherIndex){
                        requestNode.setUrl(currentUrl);
                        requestNodeList.add(requestNode);
                        requestNode =new RequestNode();
                    //B. @RequestMapping 在 method后面，临时存储一下当前的 nextRequestStart 和 nextUrl，下一次就不需要requestMappingMatcher.find
                    }else{
                        //B. 当前方法不是
                        nextUrl=currentUrl;
                        nextRequestStart=requestMappingMatcher.start();
                    }
                }
            }
        controllerNode.setRequestNodeList(requestNodeList);
        return controllerNode;
    }

}
