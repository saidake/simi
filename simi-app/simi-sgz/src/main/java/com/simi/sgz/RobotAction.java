package com.simi.sgz;

import com.simi.sgz.domain.Coordinate;

import java.awt.*;
import java.awt.event.InputEvent;

public class RobotAction extends Robot {
    public static int wt1 =300;
    public static int wt2 =500;
    public static int wt3 =800;
    public static int wt4 =1100;
    public static int wt5 =1400;
    public static int wt6 =1600;
    public static int wt7 =1800;
    public static int wt8 =2400;
    public static int wt9=2800;
    public static int wt10=3200;
    public static int wt11=3600;
    public static int wt12=4000;

    public RobotAction() throws AWTException {
    }
    public void scroll(Coordinate from, Coordinate to) {
        synchronized (RobotAction.class) {
            this.mouseMove(from);
            this.mousePress(InputEvent.BUTTON1_DOWN_MASK);
            this.mouseMove(to);
            this.mouseRelease(InputEvent.BUTTON1_DOWN_MASK);
        }
        this.sleep(wt1);
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
        this.sleep(wt9);
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
