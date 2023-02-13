package com.saidake.plugin.generate.window;

import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.OpenFileDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowFactory;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiManager;
import com.intellij.ui.content.Content;
import com.intellij.ui.content.ContentFactory;
import com.saidake.plugin.generate.xml.PomParseHandler;
import org.jetbrains.annotations.NotNull;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;

import java.io.IOException;

public class ControllerWindowFactory implements ToolWindowFactory {

    public void init(ToolWindow window) {
    }

    /**
     * 创建window
     * @param project
     * @param toolWindow
     */
    @Override
    public void createToolWindowContent(@NotNull Project project, @NotNull ToolWindow toolWindow) {
        Editor editor = FileEditorManager.getInstance(project).getSelectedTextEditor();
        VirtualFile file = FileDocumentManager.getInstance().getFile(editor.getDocument());
        int offset = editor.getCaretModel().getOffset();
        PsiElement element = PsiManager.getInstance(project).findFile(file).findElementAt(offset);

        //A. 检查pom结构
        checkPomChildren(project,project.getBaseDir().getChildren());
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
    private void checkPomChildren( Project project,@NotNull VirtualFile[] childrenList) {
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
                VirtualFile children = virtualFile.getChildren()[0];
                if("main".equals(children.getName())){
                }
                pomParseHandler.getCurrentProjectName();
            } else if(virtualFile.isDirectory()){
                checkPomChildren(project,virtualFile.getChildren());
            }
        }
    }

}
