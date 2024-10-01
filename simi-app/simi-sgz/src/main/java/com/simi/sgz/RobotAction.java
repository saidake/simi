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
    public void leftMouseClick(Coordinate coordinate, int time) {
        synchronized (RobotAction.class) {
            this.mouseMove(coordinate);
            this.mousePress(InputEvent.BUTTON1_DOWN_MASK);
            this.mouseRelease(InputEvent.BUTTON1_DOWN_MASK);
        }
        this.sleep(time);
    }
    public void leftMouseClick(Coordinate coordinate) {
        synchronized (RobotAction.class){
            this.mouseMove(coordinate);
            this.mousePress(InputEvent.BUTTON1_DOWN_MASK);
            this.mouseRelease(InputEvent.BUTTON1_DOWN_MASK);
        }
        this.sleep(WaitingTime.CLICK);
    }
    public void leftMouseClick(int milliseconds) {
        this.mousePress(InputEvent.BUTTON1_DOWN_MASK);
        this.mouseRelease(InputEvent.BUTTON1_DOWN_MASK);
        this.sleep(milliseconds);
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
