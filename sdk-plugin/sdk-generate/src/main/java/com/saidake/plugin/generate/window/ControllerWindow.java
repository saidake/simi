package com.saidake.plugin.generate.window;

import com.intellij.icons.AllIcons;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.ui.JBColor;
import com.saidake.plugin.generate.data.vo.request.ControllerInfo;
import com.saidake.plugin.generate.data.vo.request.RequestInfo;
import com.saidake.plugin.generate.data.DataHolder;
import com.saidake.plugin.generate.data.DataState;
import com.saidake.plugin.generate.data.vo.tree.UserTreeNode;
import com.saidake.plugin.generate.listener.ControllerTreeSelectionListener;

import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeSelectionModel;
import java.awt.*;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class ControllerWindow {
    private JPanel mainPanel;
    private JTree tree;
    private JScrollPane scrollPane;

    public ControllerWindow(Project project, ToolWindow toolWindow) {
        ControllerTreeSelectionListener controllerTreeSelectionListener = new ControllerTreeSelectionListener();
        tree.addTreeSelectionListener(controllerTreeSelectionListener);
        //A. 定义全局数据
        DefaultTreeModel rootTreeModel;  // JTree数据模型
        DefaultMutableTreeNode rootMutableTreeNode = null; //树根节点
        //A. 根据pom结构构建JTree
        DataState state = DataHolder.getInstance().getState();
        Set<String> pomProjectSet = state.getPomProjectList();
        Map<String, List<ControllerInfo>> projectControllerList = state.getProjectControllerList();
        Iterator<String> iterator = pomProjectSet.iterator();
        for (int i = 0; iterator.hasNext(); i++) {
            String pomItem=iterator.next();
            //B. 创建根节点
            if(i==0){
                rootMutableTreeNode=new DefaultMutableTreeNode(null);
                rootTreeModel=new DefaultTreeModel(rootMutableTreeNode);
                tree.setModel(rootTreeModel);
                tree.addTreeSelectionListener(new ControllerTreeSelectionListener());
            }
            DefaultMutableTreeNode projectNode = new DefaultMutableTreeNode(pomItem);
            List<ControllerInfo> currentProjectControllerList = projectControllerList.get(pomItem);
            if(currentProjectControllerList!=null){
                for (ControllerInfo controllerInfo : currentProjectControllerList) {
                    UserTreeNode userTreeNodeController = new UserTreeNode();
                    userTreeNodeController.setContent(controllerInfo.getTitle()==null?controllerInfo.getHeaderUrl():controllerInfo.getTitle());
                    DefaultMutableTreeNode controllerMutableTreeNode = new DefaultMutableTreeNode(userTreeNodeController);
                    for (RequestInfo requestInfo : controllerInfo.getRequestInfoList()) {
                        UserTreeNode userTreeNodeRequest = new UserTreeNode();
                        userTreeNodeRequest.setContent(requestInfo.getTitle() == null ? requestInfo.getUrl() : requestInfo.getTitle());
                        DefaultMutableTreeNode requestNode = new DefaultMutableTreeNode(userTreeNodeRequest);
                        controllerMutableTreeNode.add(requestNode);
                    }
                    projectNode.add(controllerMutableTreeNode);
                }
            }
            rootMutableTreeNode.add(projectNode);
        }
        tree.expandRow(0);
        tree.expandRow(1);
        tree.setRootVisible(false);
        tree.setRowHeight(21);
        //A. JTree全局属性设置
        TreeSelectionModel selectionModel = tree.getSelectionModel();
        selectionModel.setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        DefaultTreeCellRenderer treeCellRenderer=(DefaultTreeCellRenderer)tree.getCellRenderer();
        //ImageIcon imageIcon = new ImageIcon(ControllerWindow.class.getResource("/icon/er24.png"));
        Icon icon = AllIcons.Nodes.ModuleGroup;
        treeCellRenderer.setLeafIcon(icon);
        treeCellRenderer.setOpenIcon(icon);
        treeCellRenderer.setClosedIcon(icon);
        treeCellRenderer.setBackgroundSelectionColor(new JBColor(new Color(0, 0, 0, 0), new Color(0, 0, 0, 0)));
        treeCellRenderer.setBorderSelectionColor(new JBColor(new Color(0, 0, 0, 0), new Color(0, 0, 0, 0)));
        //treeCellRenderer.setBackgroundSelectionColor(new JBColor(new Color(0, 0, 0, 0), new Color(0, 0, 0, 0)));
    }

    public JPanel getMainPanel() {
        return mainPanel;
    }

    private void createUIComponents() {
        // TODO: place custom component creation code here
    }
}
