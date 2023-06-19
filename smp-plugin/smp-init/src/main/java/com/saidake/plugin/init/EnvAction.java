package com.saidake.plugin.init;

import com.intellij.notification.NotificationGroupManager;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.saidake.plugin.init.core.SmpInit;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.Map;
import java.util.Set;

public class EnvAction extends AnAction {

    public EnvAction(@Nullable  String text,
                     @Nullable  String description,
                     @Nullable Icon icon) {
        super(text,description,icon);
    }

    @Override
    public void actionPerformed(AnActionEvent e) {
        try {
            Map<String, Set<String>> init = SmpInit.init();
            StringBuilder stringBuilder=new StringBuilder();
            init.forEach((key,val)->{
                stringBuilder.append("<span style=\"color:#4fc3f7;\">").append(key).append(" :</span>").append("<br/>");
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
