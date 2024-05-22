package com.simi.sgz;

import java.awt.*;
import java.awt.event.InputEvent;

public class RobotAction extends Robot {
    public static int WT1 =300;
    public static int WT2 =400;
    public static int WT3 =800;
    public static int WT4 =1100;
    public static int WT5 =1400;

    public RobotAction() throws AWTException {
    }
    public void leftMouseClick(Coordinate coordinate, int time) {
        this.mouseMove(coordinate);
        this.mousePress(InputEvent.BUTTON1_DOWN_MASK);
        this.mouseRelease(InputEvent.BUTTON1_DOWN_MASK);
        this.sleep(time);
    }
    public void leftMouseClick(Coordinate coordinate) {
        this.mouseMove(coordinate);
        this.mousePress(InputEvent.BUTTON1_DOWN_MASK);
        this.mouseRelease(InputEvent.BUTTON1_DOWN_MASK);
        this.sleep(WT1);
    }
    public void leftMouseClick(int milliseconds) {
        this.mousePress(InputEvent.BUTTON1_DOWN_MASK);
        this.mouseRelease(InputEvent.BUTTON1_DOWN_MASK);
        this.sleep(milliseconds);
    }
    public void mouseMove(Coordinate coordinate){
        this.mouseMove(coordinate.x,coordinate.y);
    }
    public void sleep(int milliseconds) {
        try {
            Thread.sleep(milliseconds);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }
}
