package com.saidake.plugin.init;

import com.intellij.notification.NotificationGroupManager;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.saidake.common.util.file.SmpInit;
import com.saidake.common.util.file.support.InitException;
import com.saidake.plugin.init.core.SmpIntellijUtils;
import lombok.Getter;
import lombok.Setter;
import org.dom4j.DocumentException;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.io.IOException;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

@Getter
@Setter
public class EnvAction extends AnAction {
    private String projectName;
    private String env;
    private Boolean pomProjectNameCheck;

    public EnvAction(@Nullable String text,
                     @Nullable  String description,
                     @Nullable Icon icon,
                     String env,String projectName,Boolean pomProjectNameCheck) {
        super(text,description,icon);
        this.env=env;
        this.projectName=projectName;
        this.pomProjectNameCheck=pomProjectNameCheck;
    }

    @Override
    public void actionPerformed(AnActionEvent e) {
        try {
            Map<String, Set<String>> init = SmpInit.init(this.projectName, this.env, () -> {
                if (this.pomProjectNameCheck) {
                    try {
                        return SmpIntellijUtils.getPomProjectName(e.getProject(), projectName);
                    } catch (IOException | DocumentException ex) {
                        throw new InitException(ex.getMessage());
                    }
                } else return Optional.empty();
            });
            StringBuilder stringBuilder=new StringBuilder();
            init.forEach((key,val)->{
                stringBuilder.append("<span style=\"color:#4fc3f7;\">").append(key).append(" :").append(
                        val.isEmpty()?" empty":""
                ).append("</span>").append("<br/>");
                for (String fileName : val) {
                    stringBuilder.append("&nbsp;&nbsp;&nbsp;&nbsp;").append("<span style=\"color:#82aaff;\">").append(fileName).append("</span><br/>");
                }
            });
            NotificationGroupManager.getInstance().getNotificationGroup("Smp Notification")
                    .createNotification(stringBuilder.toString(), NotificationType.INFORMATION)
                    .setTitle("Init project success")
                    .notify(e.getProject());
        } catch (Exception ex) {
            NotificationGroupManager.getInstance().getNotificationGroup("Smp Notification")
                    .createNotification(ex.getMessage(), NotificationType.ERROR)
                    .setTitle("Init project failed")
                    .notify(e.getProject());
        }
    }
}
