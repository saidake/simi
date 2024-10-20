package com.simi.sgz.action;

import com.simi.sgz.AAAconfig.WaitingTime;

import java.awt.*;
import java.awt.event.InputEvent;

public class RobotAction extends Robot {

    public RobotAction() throws AWTException {
    }
    public void scroll(int[] from, int[] to) {
        synchronized (RobotAction.class) {
            this.mouseMove(from);
            this.mousePress(InputEvent.BUTTON1_DOWN_MASK);
            this.mouseMove(to);
            this.mouseRelease(InputEvent.BUTTON1_DOWN_MASK);
        }
        this.sleep(WaitingTime.SCROLL);
    }
    public void leftMouseClickEx(int[] coordinate, int time) {
        synchronized (RobotAction.class) {
            this.leftMouseClick(coordinate);
        }
        this.sleep(time);
    }
    public void leftMouseClickMark(int[] mark, int[] btn) {
        //select mark but only focus on moving view.
        synchronized (RobotAction.class) {
            this.leftMouseClick(mark);
        }
        this.sleep(WaitingTime.SELECT_MARK);
        //select mark and click action button (double refresh).
        synchronized (RobotAction.class) {
            this.leftMouseClick(mark);
            this.sleep(WaitingTime.REFRESH_MARK);
            this.leftMouseClick(btn);
            this.sleep(WaitingTime.CLICK);
            this.leftMouseClick(mark);
            this.sleep(WaitingTime.REFRESH_MARK_MID);
            this.leftMouseClick(btn);
        }
        this.sleep(WaitingTime.SELECT_MARK);
    }
    public void leftMouseClickEx(int[] coordinate) {
        synchronized (RobotAction.class){
            this.leftMouseClick(coordinate);
        }
        this.sleep(WaitingTime.CLICK);
    }
    public void leftMouseClick(int[] coordinate) {
        this.mouseMove(coordinate);
        this.mousePress(InputEvent.BUTTON1_DOWN_MASK);
        this.mouseRelease(InputEvent.BUTTON1_DOWN_MASK);
    }
    public void mouseMove(int[] coordinate){
        this.mouseMove(coordinate[0],coordinate[1]);
    }
    public void sleep(int milliseconds) {
        try {
            Thread.sleep(milliseconds);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }
}
