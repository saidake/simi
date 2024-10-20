package com.simi.sgz.action;


import lombok.AllArgsConstructor;

import java.util.Arrays;


@AllArgsConstructor
public class LevelUPTask extends Thread {
    private int mainCityArmyNumber;
    private int secondCityArmyNumber;

    private RobotAction robot;
    private TroopOperation operation;
    private int[] staminaList;
    private boolean[] supplyList;
    int[] clearMarkList;
    int[] clearTabList;
    int waitingTime;
    @Override
    public void run() {
        int totalArmyNumber=mainCityArmyNumber+secondCityArmyNumber;
        //A. Check if there is a need to supply the army.
        boolean hasStamina = Arrays.stream(staminaList).anyMatch(item -> item > 15);
        boolean needSupplyForAny=false;
        for (boolean curSupply : supplyList) {
            if (curSupply) {
                needSupplyForAny = true;
                break;
            }
        }
        //A.start the level-up action
        boolean hasAnyStaminaLeft=true;
        while (hasAnyStaminaLeft){
            hasAnyStaminaLeft=false;
            //B. reduce stamina
            for (int i = 0; i < totalArmyNumber; i++) {
                staminaList[i]-=15;
                if(staminaList[i]>=15)hasAnyStaminaLeft=true;
            }
            //B. enter city and supply army
            if(needSupplyForAny&&hasStamina){
                boolean enteredCity=false;
                boolean enteredSecondCity=false;
                for (int i = 0; i < totalArmyNumber; i++) {
                    if(supplyList[i]&&staminaList[i]>=0){
                        if(!enteredCity){
                            operation.enterCity(robot,i);
                            enteredCity=true;
                        }
                        //C. enter the second city.
                        if(!enteredSecondCity&&i>=mainCityArmyNumber){
                            operation.goBackAndScrollToBottom(robot);
                            operation.enterCity(robot,i);
                            enteredSecondCity=true;
                        }
                        operation.supplyArmy(robot,mainCityArmyNumber, i);
                    }
                }
                //B. go to the mark button.
                operation.goBackAndScrollToBottom(robot);
            }

            //B. enter city and supply army
            for (int i = 0; i < totalArmyNumber; i++) {
                if(staminaList[i]<0)continue;
                //A。AWT operations
                operation.clear(robot, mainCityArmyNumber, i, clearMarkList[i], clearTabList[i]);
            }
            try {
                int adjustedWaitingTime=waitingTime*1000+3000;
                Thread.sleep(adjustedWaitingTime+adjustedWaitingTime/2);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }
    }
}
