package com.simi.sgz;

import com.simi.sgz.AAAconfig.WaitingTime;
import com.simi.sgz.domain.Coordinate;

import java.awt.*;
import java.awt.event.InputEvent;

public class RobotAction extends Robot {

    public RobotAction() throws AWTException {
    }
    public void scroll(Coordinate from, Coordinate to) {
        synchronized (RobotAction.class) {
            this.mouseMove(from);
            this.mousePress(InputEvent.BUTTON1_DOWN_MASK);
            this.mouseMove(to);
            this.mouseRelease(InputEvent.BUTTON1_DOWN_MASK);
        }
        this.sleep(WaitingTime.SCROLL);
    }
    public void leftMouseClickEx(Coordinate coordinate, int time) {
        synchronized (RobotAction.class) {
            this.leftMouseClick(coordinate);
        }
        this.sleep(time);
    }
    public void leftMouseClickMark(Coordinate mark, Coordinate btn, int time, int refreshTime) {
        //select mark but only focus on moving view.
        synchronized (RobotAction.class) {
            this.leftMouseClick(mark);
        }
        this.sleep(time);
        //select mark and click action button (double refresh).
        synchronized (RobotAction.class) {
            this.leftMouseClick(mark);
            this.sleep(refreshTime);
            this.leftMouseClick(mark);
            this.leftMouseClick(btn);
            this.sleep(time);
            this.leftMouseClick(btn);
        }
        this.sleep(time);
    }
    public void leftMouseClickEx(Coordinate coordinate) {
        synchronized (RobotAction.class){
            this.leftMouseClick(coordinate);
        }
        this.sleep(WaitingTime.CLICK);
    }
    public void leftMouseClick(Coordinate coordinate) {
        this.mouseMove(coordinate);
        this.mousePress(InputEvent.BUTTON1_DOWN_MASK);
        this.mouseRelease(InputEvent.BUTTON1_DOWN_MASK);
    }
    public void mouseMove(Coordinate coordinate){
        this.mouseMove(coordinate.getX(),coordinate.getY());
    }
    public void sleep(int milliseconds) {
        try {
            Thread.sleep(milliseconds);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }
}
