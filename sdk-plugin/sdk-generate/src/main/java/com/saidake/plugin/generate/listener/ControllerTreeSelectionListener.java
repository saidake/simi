package com.saidake.plugin.generate.listener;

import com.saidake.plugin.generate.data.tree.UserTreeNode;

import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;

public class ControllerTreeSelectionListener implements TreeSelectionListener {
    @Override
    public void valueChanged(TreeSelectionEvent e) {
        DefaultMutableTreeNode lastPathComponent = (DefaultMutableTreeNode)e.getPath().getLastPathComponent();
        lastPathComponent.getUserObject();
    }
}
