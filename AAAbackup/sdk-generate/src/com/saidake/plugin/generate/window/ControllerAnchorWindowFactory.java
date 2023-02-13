package com.saidake.plugin.generate.window;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.openapi.wm.ToolWindowFactory;
import com.intellij.ui.content.Content;
import com.intellij.ui.content.ContentFactory;
import org.jetbrains.annotations.NotNull;

public class ControllerAnchorWindowFactory implements ToolWindowFactory {

    public void init(ToolWindow window) {
    }

    @Override
    public void createToolWindowContent(@NotNull Project project, @NotNull ToolWindow toolWindow) {
        ControllerAnchorWindow controllerAnchorWindow=new ControllerAnchorWindow(project,toolWindow);
        ContentFactory instance = ContentFactory.SERVICE.getInstance();
        Content content = instance.createContent(controllerAnchorWindow.getMainPanel(),"",false);
        toolWindow.getContentManager().addContent(content);
    }

}
