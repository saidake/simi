package com.simi.sgz;

import com.simi.sgz.AAAconfig.LevelUPTask;
import com.simi.sgz.domain.*;

import java.awt.*;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicBoolean;

public class LevelUP {
    public static void main(String[] args) throws AWTException {
        // Prerequisites
        //!. All troops have returned to the city.
        //2. There are no enemy troops around the city.
        // 15  30  45  60  75  90  105  120
        int minus=0;
        int[][] staminaList = new int[][]{
                {45,60,60,90,60,     0, 0,0,0 },
                { 60,45,45,75,45,    0, 0,0,0 },
                { 60,45,60,75,75,    0, 0,0,0 },
                { 60,45,0,0,0,        0, 0,0,0 },
        };
        boolean[][] supplyList = new boolean[][]{
                {true, true, false,false,false,     false, false, false,false},
                {true, true, false,false, false,    false, false,false,false},
                {true, true, false,false,false,      false, false, false,false},
                {true, false, false,false,false,      false, false, false,false},
        };
        int[][] clearMarkList = new int[][]{
                {1, 2, 3,3,3,  3,3,3,4 },
                {1, 1, 3,3,3,  3,3,3,4},
                {2, 2, 3,3,3,  3,3,3,4},
                {1, 2, 3,2,3,  3,3,3,4}
        };
        int[][] clearTabList = new int[][]{
                {1, 1, 1, 1, 1,  1,1,1,1},
                {1, 1, 2, 2, 2,  1,1,1,1},
                {2, 2, 1, 1, 1,  1,1,1,1},
                {1, 1, 1, 1, 1,  1,1,1,1}
        };
        int[] waitingTimeList=new int[]{60, 75, 45, 75};


        RobotAction robot = new RobotAction();
        List<Operation> operationList = Arrays.asList(Pineapple.builder().build(), Orange.builder().build(), Durian.builder().build(), Grape.builder().build());
        if(minus!=0){
            for (int i = 0; i < staminaList.length; i++) {
                for (int j = 0; j < staminaList[0].length; j++) {
                    staminaList[i][j]-=minus;
                }
            }
        }
        // 30 45 60 75 90 105 120
        for (int i = 0; i < 4; i++) {
            LevelUPTask levelUPTask = new LevelUPTask(
                    robot,
                    operationList.get(i),
                    staminaList[i],
                    supplyList[i],
                    clearMarkList[i],
                    clearTabList[i],
                    waitingTimeList[i]
            );
            levelUPTask.start();
        }
    }
}
