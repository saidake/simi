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
 * @author  Zou, Hao David
 * @see     ControllerWindow
 * @since   1.4
 */
public class ControllerWindowFactory implements ToolWindowFactory {

    private Logger logger=Logger.getLogger(ControllerWindowFactory.class.getName());

    public void init(ToolWindow window) {
    }
    /**
     * 设置窗口 ControllerWindow 的属性
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
     * 检查pom.xml文件，用{@link VirtualFile}。
     * 检查module项目结构
     *
     * @param childrenList 子文件列表 {@link VirtualFile}.
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
                //B. 检查controller项目结构
                checkSrcFolder(virtualFile,currentProjectName);
            } else if(virtualFile.isDirectory()){
                //B. 检查module项目结构
                handleInitBaseData(project,virtualFile.getChildren());
            }
        }
    }

    /**
     * 检查src文件夹，检查Controller信息
     * @param virtualFile
     * @param currentProjectName
     */
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
        ControllerNode controllerNode =new ControllerNode();
        String packagePath=null;
        String className=null;
        System.out.println("controllerFile: "+child.getName());
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
                controllerNode.setTitle(springDocTagMatcher.group(2));
            }
        }else{
            logger.severe("cannot find class");
        }
        controllerNode.setPackagePath(packagePath+"."+className);
        controllerNode.setFilePath(child.getPath());

        //A. 获取method方法名 List
        System.out.println("========================================test method");
        String nextUrl=null;
        Integer nextRequestStart=null;
        String nextSummary=null;
        String nextDescription=null;
        Integer nextOperationStart=null;
        RequestNode requestNode =new RequestNode();
        List<RequestNode> requestNodeList =new ArrayList<>();
        while (methodMatcher.find()){
            System.out.println("fileName: "+child.getName());
            String methodName = methodMatcher.group(4);
            System.out.println("methodName: "+methodName);
            requestNode.getMethodNode().setPackagePath(packagePath+"."+methodName);            //A. 检查 title
            //A. 已经检查过一次了
            if(nextSummary!=null&&nextOperationStart<methodMatcher.start()){
                requestNode.setTitle(nextSummary);
                requestNode.setDescription(nextDescription);
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
                    requestNode.setTitle(summary);
                    requestNode.setDescription(description);
                }else{
                    //B. 当前方法不是
                    nextSummary=summary;
                    nextDescription=description;
                    nextOperationStart=springDocOperationMatcher.start();
                }
            }

            //A. IF(nextUrl) 已经检查过一次了
            if(nextUrl!=null&&nextRequestStart<methodMatcher.start()) {
                requestNode.setUrl(nextUrl);
                requestNode.setMethodNode(new MethodNode(packagePath,child.getPath()));
                requestNodeList.add(requestNode);
                requestNode =new RequestNode();
                nextUrl=null;
                nextRequestStart=null;
            //A. ELSE IF:   [CORE]检查 url
            } else if(requestMappingMatcher.find()){
                    String currentUrl = requestMappingMatcher.group(4) == null ? requestMappingMatcher.group(6) : requestMappingMatcher.group(4);
                    if(controllerNode.getPrefixUrl()!=null)currentUrl= controllerNode.getPrefixUrl()+currentUrl;
                    if(requestMappingMatcher.start()<methodMatcher.start()){
                        requestNode.setUrl(currentUrl);
                        requestNodeList.add(requestNode);
                        requestNode =new RequestNode();
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
