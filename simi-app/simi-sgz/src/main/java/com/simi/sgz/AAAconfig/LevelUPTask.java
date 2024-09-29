package com.simi.sgz.AAAconfig;


import com.simi.sgz.RobotAction;
import com.simi.sgz.domain.Operation;
import lombok.AllArgsConstructor;


@AllArgsConstructor
public class LevelUPTask extends Thread {
    private final int mainCityArmyNumber=5;
    private final int secondCityArmyNumber=4;
    private final int totalArmyNumber=9;

    private RobotAction robot;
    private Operation operation;
    private int[] staminaList;
    private boolean[] supplyList;
    int[] clearMarkList;
    int[] clearTabList;
    int waitingTime;
    @Override
    public void run() {
        //A. Check if there is a need to supply the army.
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
            if(needSupplyForAny){
                operation.enterCity(robot);
                for (int i = 0; i < totalArmyNumber; i++) {
                    if(supplyList[i]&&staminaList[i]>=0) operation.supplyArmy(robot,mainCityArmyNumber, i);
                }
            }
            //B. go to the mark button.
            operation.goBackAndScrollToBottom(robot);
            //B. enter city and supply army
            for (int i = 0; i < totalArmyNumber; i++) {
                if(staminaList[i]<0)continue;
                //A。AWT operations
                operation.clear(robot, mainCityArmyNumber, i, clearMarkList[i], clearTabList[i]);
                try {
                    int adjustedWaitingTime=waitingTime+10;
                    Thread.sleep(adjustedWaitingTime+adjustedWaitingTime/2);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
            }

        }
    }
}
