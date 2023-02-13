package com.saidake.plugin.generate.window;

import com.intellij.icons.AllIcons;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.saidake.plugin.generate.data.DataHolder;
import com.saidake.plugin.generate.data.DataState;

import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeSelectionModel;
import java.util.Iterator;
import java.util.Set;

public class ControllerWindow {
    private JPanel mainPanel;
    private JTree tree;

    public ControllerWindow(Project project, ToolWindow toolWindow) {
        //A. 定义全局数据
        DefaultTreeModel rootTreeModel;  // JTree数据模型
        DefaultMutableTreeNode rootMutableTreeNode = null; //树根节点
        //A. 根据pom结构构建JTree
        DataState state = DataHolder.getInstance().getState();
        Set<String> pomProjectSet = state.getPomProjectList();
        Iterator<String> iterator = pomProjectSet.iterator();
        for (int i = 0; iterator.hasNext(); i++) {
            String pomItem=iterator.next();
            //B. 创建根节点
            if(i==0){
                rootMutableTreeNode=new DefaultMutableTreeNode(null);
                rootTreeModel=new DefaultTreeModel(rootMutableTreeNode);
                tree.setModel(rootTreeModel);
            }
            rootMutableTreeNode.add(new DefaultMutableTreeNode( pomItem));
        }
        tree.expandRow(0);
        tree.setRootVisible(false);
        //A. JTree全局属性设置
        TreeSelectionModel selectionModel = tree.getSelectionModel();
        selectionModel.setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        DefaultTreeCellRenderer treeCellRenderer=(DefaultTreeCellRenderer)tree.getCellRenderer();
        //ImageIcon imageIcon = new ImageIcon(ControllerWindow.class.getResource("/icon/er24.png"));
        Icon icon = AllIcons.Nodes.ModuleGroup;
        treeCellRenderer.setLeafIcon(icon);
        treeCellRenderer.setOpenIcon(icon);
        treeCellRenderer.setClosedIcon(icon);
        //treeCellRenderer.setBackgroundSelectionColor(new JBColor(new Color(0, 0, 0, 0), new Color(0, 0, 0, 0)));
    }

    public JPanel getMainPanel() {
        return mainPanel;
    }

    private void createUIComponents() {
        // TODO: place custom component creation code here
    }
}
