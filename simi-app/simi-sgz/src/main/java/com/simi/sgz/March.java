package com.simi.sgz;

import java.awt.*;
import java.util.HashMap;
import java.util.Map;

import static com.simi.sgz.Coordinate.*;
import static com.simi.sgz.RobotAction.*;

public class March {
    private static int [] markLocationList=new int[3];
    private static Map<Integer, Coordinate> PI_BTN_MAP=new HashMap<>(){{
        put(1,PI_BT1);
        put(2,PI_BT2);
    }};
    private static Map<Integer, Coordinate> PI_TAB_MAP=new HashMap<>(){{
        put(1,PI_TAB1);
        put(2,PI_TAB2);
        put(3,PI_TAB3);
    }};
    private static Map<Integer, Coordinate> PI_ARMY_MAP=new HashMap<>(){{
        put(11,PI_ARMY1_FROM1);
    }};
    private static Map<Integer, Coordinate> PI_MARK_MAP=new HashMap<>(){{
        put(1,PI_MARK1);
        put(2,PI_MARK2);
        put(3,PI_MARK3);
    }};
    private static Map<Integer, Coordinate> PU_BTN_MAP=new HashMap<>(){{
        put(1,PU_BT1);
        put(2,PU_BT2);
    }};
    private static Map<Integer, Coordinate> PU_TAB_MAP=new HashMap<>(){{
        put(1,PU_TAB1);
        put(2,PU_TAB2);
        put(3,PU_TAB3);
    }};
    private static Map<Integer, Coordinate> PU_ARMY_MAP=new HashMap<>(){{
        put(11,PU_ARMY1_FROM1);
        put(22,PU_ARMY2_FROM2);
    }};
    private static Map<Integer, Coordinate> PU_MARK_MAP=new HashMap<>(){{
        put(1,PU_MARK1);
        put(2,PU_MARK2);
        put(3,PU_MARK3);
    }};
    private static Map<Integer, Coordinate> DU_BTN_MAP=new HashMap<>(){{
        put(1,DU_BT1);
        put(2,DU_BT2);
    }};
    private static Map<Integer, Coordinate> DU_TAB_MAP=new HashMap<>(){{
        put(1,DU_TAB1);
        put(2,DU_TAB2);
        put(3,DU_TAB3);
    }};
    private static Map<Integer, Coordinate> DU_ARMY_MAP=new HashMap<>(){{
        put(11,DU_ARMY1_FROM1);
    }};
    private static Map<Integer, Coordinate> DU_MARK_MAP=new HashMap<>(){{
        put(1,DU_MARK1);
        put(2,DU_MARK2);
        put(3,DU_MARK3);

    }};
    public static void main(String[] args) throws InterruptedException, AWTException {
        // reset screen
        RobotAction robot = new RobotAction();
        //====================================  daily attack a city.

        //====================================  march
//        pineappleMarch(robot,1,1,11,1);
//        pineappleMarch(robot,2,1,11, 1);
//        pineappleMarch(robot,1,1,11, 1);
//        pumpkinMarch(robot,2,1,11,1);
//        pumpkinMarch(robot,1,1,11,2);
//        durianMarch(robot,2,1,11,1);
        //====================================  march to red point
        pineappleMarch(robot,1,1,11); // 刘备
        pineappleMarch(robot,1,1,11); // 张辽  01:05
        pumpkinMarch(robot,1,1,11);  // 甘宁
        pumpkinMarch(robot,1,1,11);  // 曹操  01:02
//        durianMarch(robot,1,1,11);    // 黄忠
//        durianMarch(robot,1,1,11);    // 程昱
//        Thread.sleep(10000);
//        pumpkinMarch(robot,1,1,11);
//        Thread.sleep(3000);
//        pineappleMarch(robot,1,1,11);
        //retreat(robot);
        //====================================  go back
//        pineappleStopAndMarchToMark(robot,1,1,11,2);
//        pumpkinStopAndMarchToMark(robot,1,1,11,2);
//        durianStopAndMarchToMark(robot,1,1,11,2);
        //==================================== prepare to retreat

    }

    private static void retreat(RobotAction robot) {
        resetScreenToMark(robot,1);
        pineappleMarch(robot,1,1,11);
        pumpkinMarch(robot,1,1,11);
        pumpkinMarch(robot,1,1,11);
        durianMarch(robot,1,1,11);
    }


    private static void resetScreenToMark(RobotAction robot, int mark) {
        robot.leftMouseClick(PI_MARK_MAP.get(mark));
        robot.leftMouseClick(PU_MARK_MAP.get(mark));
        robot.leftMouseClick(DU_MARK_MAP.get(mark));
        robot.sleep(1400);
    }



    private static void pineappleStopAndMarchToMark(RobotAction robot, int btn, int tab, int army, int mark) {
        // select location
        robot.leftMouseClick(PI_RED_POINT);
        // stop button
        robot.leftMouseClick(PI_STOP_BT1);
        // right tab list
        robot.leftMouseClick(PI_SMALL_CONFIRM);
        // go to mark1
        robot.leftMouseClick(PI_MARK1,WT3);
        pineappleMarch(robot,btn,tab,army,mark);
    }
    private static void pumpkinStopAndMarchToMark(RobotAction robot, int btn, int tab, int army, int mark) {
        // select location
        robot.leftMouseClick(PU_RED_POINT);
        // stop button
        robot.leftMouseClick(PU_STOP_BT1);
        // right tab list
        robot.leftMouseClick(PU_SMALL_CONFIRM);
        // go to mark1
        robot.leftMouseClick(PU_MARK1,WT3);
        pumpkinMarch(robot,btn,tab,army,mark);

    }
    private static void durianStopAndMarchToMark(RobotAction robot, int btn, int tab, int army, int mark) {
        // select location
        robot.leftMouseClick(DU_RED_POINT);
        // stop button
        robot.leftMouseClick(DU_STOP_BT1);
        // right tab list
        robot.leftMouseClick(DU_SMALL_CONFIRM);
        // go to mark1
        robot.leftMouseClick(DU_MARK1,WT3);
        durianMarch(robot,btn,tab,army,mark);

    }
    private static void pineappleMarch(RobotAction robot, int btn, int tab, int army) {

        // select location
        robot.leftMouseClick(PI_RED_POINT);
        // march button
        robot.leftMouseClick(PI_BTN_MAP.get(btn));
        // right tab list
        robot.leftMouseClick(PI_TAB_MAP.get(tab));
        // select army
        robot.leftMouseClick(PI_ARMY_MAP.get(army));
        // start march
        robot.leftMouseClick(PI_CONFIRM, WT2);
    }
    private static void pineappleMarch(RobotAction robot, int btn, int tab, int army, int mark){
        int i = markLocationList[0];
        if(i!=mark)markLocationList[0]=mark;
        // select location
        robot.leftMouseClick(PI_MARK_MAP.get(mark), i==mark?WT1:WT4);
        // march button
        robot.leftMouseClick(PI_BTN_MAP.get(btn));
        // right tab list
        robot.leftMouseClick(PI_TAB_MAP.get(tab));
        // select army
        robot.leftMouseClick(PI_ARMY_MAP.get(army));
        // start march
        robot.leftMouseClick(PI_CONFIRM, WT2);
    }
    private static void pumpkinMarch(RobotAction robot, int btn, int tab, int army) {
        // select location
        robot.leftMouseClick(PU_RED_POINT);
        // march button
        robot.leftMouseClick(PU_BTN_MAP.get(btn));
        // right tab list
        robot.leftMouseClick(PU_TAB_MAP.get(tab));
        // select army
        robot.leftMouseClick(PU_ARMY_MAP.get(army));
        // start march
        robot.leftMouseClick(PU_CONFIRM, WT2);
    }
    private static void pumpkinMarch(RobotAction robot, int btn, int tab, int army, int mark) {
        int i = markLocationList[1];
        if(i!=mark)markLocationList[1]=mark;
        // select location
        robot.leftMouseClick(PU_MARK_MAP.get(mark), i==mark?WT1:WT4);
        // march button
        robot.leftMouseClick(PU_BTN_MAP.get(btn));
        // right tab list
        robot.leftMouseClick(PU_TAB_MAP.get(tab));
        // select army
        robot.leftMouseClick(PU_ARMY_MAP.get(army));
        // start march
        robot.leftMouseClick(PU_CONFIRM, WT2);
    }
    private static void durianMarch(RobotAction robot, int btn, int tab, int army) {
        // select location
        robot.leftMouseClick(DU_RED_POINT);
        // march button
        robot.leftMouseClick(DU_BTN_MAP.get(btn));
        // right tab list
        robot.leftMouseClick(DU_TAB_MAP.get(tab));
        // select army
        robot.leftMouseClick(DU_ARMY_MAP.get(army));
        // start march
        robot.leftMouseClick(DU_CONFIRM, WT2);
    }
    private static void durianMarch(RobotAction robot, int btn, int tab, int army, int mark) {
        int i = markLocationList[2];
        if(i!=mark)markLocationList[2]=mark;
        // select location
        robot.leftMouseClick(DU_MARK_MAP.get(mark), i==mark?WT1:WT4);
        // march button
        robot.leftMouseClick(DU_BTN_MAP.get(btn));
        // right tab list
        robot.leftMouseClick(DU_TAB_MAP.get(tab));
        // select army
        robot.leftMouseClick(DU_ARMY_MAP.get(army));
        // start march
        robot.leftMouseClick(DU_CONFIRM, WT2);
    }
}
