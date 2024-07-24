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

    public RobotAction() throws AWTException {
    }
    public void scroll(Coordinate from, Coordinate to) {
        this.mouseMove(from);
        this.mousePress(InputEvent.BUTTON1_DOWN_MASK);
        this.mouseMove(to);
        this.mouseRelease(InputEvent.BUTTON1_DOWN_MASK);
        this.sleep(wt1);
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
        this.sleep(wt3);
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
