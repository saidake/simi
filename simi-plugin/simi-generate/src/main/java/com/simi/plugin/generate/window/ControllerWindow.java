package com.simi.plugin.generate.window;

import com.intellij.icons.AllIcons;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.wm.ToolWindow;
import com.intellij.ui.JBColor;
import com.simi.plugin.generate.data.vo.node.ControllerNode;
import com.simi.plugin.generate.data.vo.node.MethodNode;
import com.simi.plugin.generate.data.vo.node.RequestNode;
import com.simi.plugin.generate.data.core.DataHolder;
import com.simi.plugin.generate.data.core.DataState;
import com.simi.plugin.generate.listener.ControllerTreeSelectionListener;

import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeSelectionModel;
import java.awt.*;
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
        //A. common data
        DefaultTreeModel rootTreeModel;  // JTree data model
        DefaultMutableTreeNode rootMutableTreeNode = null; //tree root node
        //A. init root node
        rootMutableTreeNode=new DefaultMutableTreeNode(null);
        rootTreeModel=new DefaultTreeModel(rootMutableTreeNode);
        tree.setModel(rootTreeModel);
        tree.addTreeSelectionListener(new ControllerTreeSelectionListener());
        //if(DataHolder.getInstance().getState().getControllerNodeMap().isEmpty()) handleAppendControllerNode(rootMutableTreeNode);
        handleAppendControllerNode(rootMutableTreeNode);
        tree.expandRow(0);
        tree.expandRow(1);
        tree.expandRow(2);
        tree.expandRow(3);
        tree.setRootVisible(false);
        tree.setRowHeight(21);
        //A. JTree global attribute setting
        TreeSelectionModel selectionModel = tree.getSelectionModel();
        selectionModel.setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        DefaultTreeCellRenderer treeCellRenderer=(DefaultTreeCellRenderer)tree.getCellRenderer();
        //ImageIcon imageIcon = new ImageIcon("/icon/deer.ico");
        Icon icon = AllIcons.Nodes.ConfigFolder;
        treeCellRenderer.setLeafIcon(icon);
        treeCellRenderer.setOpenIcon(icon);
        treeCellRenderer.setClosedIcon(icon);
        treeCellRenderer.setBackgroundSelectionColor(new JBColor(new Color(0, 0, 0, 0), new Color(0, 0, 0, 0)));
        treeCellRenderer.setBorderSelectionColor(new JBColor(new Color(0, 0, 0, 0), new Color(0, 0, 0, 0)));
    }

    private static void handleAppendControllerNode(DefaultMutableTreeNode rootMutableTreeNode) {
        DataState dataState = DataHolder.getInstance().getState();
        Set<String> pomProjectList = dataState.getPomProjectList();
        Map<String, ControllerNode> controllerNodeMap = dataState.getControllerNodeMap();
        Map<String, RequestNode> requestNodeMap = dataState.getRequestNodeMap();
        Map<String, List<ControllerNode>> projectControllerList = dataState.getProjectControllerList();
        //B. foreach pom project list
        for (String pomProject : pomProjectList) {
            DefaultMutableTreeNode pomProjectNode = new DefaultMutableTreeNode(pomProject);
            //B. add panel info
            List<ControllerNode> currentProjectControllerList = projectControllerList.get(pomProject);
            if(currentProjectControllerList==null||currentProjectControllerList.isEmpty())continue;
            for (ControllerNode controllerNode : currentProjectControllerList) {
                String controllerText;
                if(controllerNode.getSpringDocTag()!=null){
                    controllerText=controllerNode.getSpringDocTag();
                }else if(controllerNode.getPrefixUrl()!=null){
                    controllerText=controllerNode.getPrefixUrl();
                }else{
                    controllerText=controllerNode.getFileName().split("\\.")[0];
                }
                controllerNodeMap.put(controllerText,controllerNode);
                DefaultMutableTreeNode controllerMutableTreeNode = new DefaultMutableTreeNode(controllerText);
                for (RequestNode requestNode : controllerNode.getRequestNodeList()) {
                    String requestText;
                    if(requestNode.getSpringDocOperation()!=null){
                        requestText=requestNode.getSpringDocOperation();
                    }else if(requestNode.getUrl()!=null){
                        requestText=requestNode.getUrl();
                    }else{
                        requestText=requestNode.getMethodNode().getMethodName();
                    }
                    requestNodeMap.put(requestText,requestNode);
                    DefaultMutableTreeNode requestMutableTreeNode = new DefaultMutableTreeNode(requestText);
                    for (MethodNode sonMethodNode : requestNode.getMethodNode().getChildMethodNodeList()) {
                        requestMutableTreeNode.add(new DefaultMutableTreeNode(sonMethodNode.getMethodName()));
                    }
                    controllerMutableTreeNode.add(requestMutableTreeNode);
                }
                pomProjectNode.add(controllerMutableTreeNode);
            }
            rootMutableTreeNode.add(pomProjectNode);
        }
    }

    public JPanel getMainPanel() {
        return mainPanel;
    }

    private void createUIComponents() {
        // TODO: place custom component creation code here
    }
}
