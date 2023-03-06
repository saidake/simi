package com.saidake.plugin.generate.window;

import com.intellij.icons.AllIcons;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.ui.JBColor;
import com.saidake.plugin.generate.data.vo.node.ControllerNode;
import com.saidake.plugin.generate.data.vo.node.MethodNode;
import com.saidake.plugin.generate.data.vo.node.RequestNode;
import com.saidake.plugin.generate.data.core.DataHolder;
import com.saidake.plugin.generate.data.core.DataState;
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
        DataState dataState = DataHolder.getInstance().getState();
        Set<String> pomProjectSet = dataState.getPomProjectList();
        Map<String, ControllerNode> controllerNodeMap = dataState.getControllerNodeMap();
        Map<String, RequestNode> requestNodeMap = dataState.getRequestNodeMap();
        Map<String, MethodNode> methodNodeMap = dataState.getMethodNodeMap();
        Map<String, List<ControllerNode>> projectControllerList = dataState.getProjectControllerList();
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
            //B. 添加 面板信息
            List<ControllerNode> currentProjectControllerList = projectControllerList.get(pomItem);
            if(currentProjectControllerList!=null){
                for (ControllerNode controllerNode : currentProjectControllerList) {
                    String controllerText=controllerNode.getTitle()==null?controllerNode.getPrefixUrl():controllerNode.getPackagePath();
                    controllerNodeMap.put(controllerText,controllerNode);
                    DefaultMutableTreeNode controllerMutableTreeNode = new DefaultMutableTreeNode(controllerText);
                    for (RequestNode requestNode : controllerNode.getRequestNodeList()) {
                        String requestText=requestNode.getTitle()==null?requestNode.getUrl():requestNode.getPackagePath();
                        requestNodeMap.put(requestText,requestNode);
                        DefaultMutableTreeNode requestMutableTreeNode = new DefaultMutableTreeNode(requestText);
                        controllerMutableTreeNode.add(requestMutableTreeNode);
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
