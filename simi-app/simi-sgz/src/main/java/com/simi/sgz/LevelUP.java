package com.simi.sgz;

import com.simi.sgz.domain.Operation;
import com.simi.sgz.domain.Pineapple;
import com.simi.sgz.domain.Orange;
import com.simi.sgz.domain.Durian;

import java.awt.*;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicBoolean;

public class LevelUP {
    public static void main(String[] args) throws AWTException {
        RobotAction robot = new RobotAction();
        List<Operation> operationList = Arrays.asList(new Pineapple(), new Orange(), new Durian());
        int mainCityArmyNumber=5;
        int secondCityArmyNumber=4;
        // 15  30  45  60  75  90  105  120
        int minus=0;
        int[][] staminaList = new int[][]{
                {120,120,120,0,0,            0, 0, 0,0 },
                { 120,120,120,0,0,           0, 0,0,0},
                { 120,120,120,0,0,           0, 0,0,0 }
        };
        boolean[][] supplyList = new boolean[][]{
                {true, false, false,false,false,     false, false, false,false},
                {true, false, false,false, false,    false, false,false,false},
                {true, false, false,false,false,      false, false, false,false} };
        int[][] clearMarkList = new int[][]{
                {1, 2, 3,2,3,  3,3,3,4 },
                {1, 2, 3,2,4,  3,3,3,4},
                {1, 2, 3,2,3,  3,3,3,4}
        };
        int[][] clearTabList = new int[][]{
                {1, 1, 1, 1, 2,  1,1,1,1},
                {1, 1, 1, 1, 2,  1,1,1,1},
                {1, 1, 1, 1, 1, 1,1,1,1}
        };
        int[] timeList=new int[]{93, 93, 93};
        boolean[][] failedPassList=new boolean[3][mainCityArmyNumber+secondCityArmyNumber];
        boolean[] passedList=new boolean[3];
        boolean anyPassed=true;
        ExecutorService executorService = Executors.newFixedThreadPool(3);
        AtomicBoolean pineappleWaiting=new AtomicBoolean(false);
        AtomicBoolean orangeWaiting=new AtomicBoolean(false);
        AtomicBoolean durianWaiting=new AtomicBoolean(false);
        if(minus!=0){
            for (int i = 0; i < staminaList.length; i++) {
                for (int j = 0; j < staminaList[0].length; j++) {
                    staminaList[i][j]-=minus;
                }
            }
        }
        // 30 45 60 75 90 105 120
        while (anyPassed){
            Arrays.fill(passedList,false);
            anyPassed=false;
            for (int i = 0; i < staminaList.length; i++) {
                for (int j = 0; j < staminaList[i].length; j++) {
                    staminaList[i][j]-=15;
                    if(staminaList[i][j]<0)failedPassList[i][j]=true;
                    else {
                        passedList[i]=true;
                        anyPassed=true;
                    }
                }
                Integer waitIndex=8;
                Integer waitIndex2=null;
                if(passedList[i]){
                    Operation operation = operationList.get(i);
                    operation.enterCity(robot);
                    operation.supplyArmy(robot, failedPassList[i], supplyList[i]);
                    operation.goBack(robot);
                    operation.clear(robot, mainCityArmyNumber, failedPassList[i], clearMarkList[i], clearTabList[i], waitIndex, waitIndex2);
                }
            }
            int maxTime=0;
            for (int i = 0; i < timeList.length; i++) {
                int curTime = timeList[i] * (passedList[i]?1:0);
                if(maxTime<curTime)maxTime=curTime;
            }
            robot.sleep(maxTime*1000);
        }
    }
}
