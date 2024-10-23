package com.simi.sgz.tasks;


import com.simi.sgz.action.RobotAction;
import com.simi.sgz.action.TroopOperation;
import com.simi.sgz.domain.properties.CoordinatesReader;
import com.simi.sgz.domain.properties.SimiSgz;
import lombok.NoArgsConstructor;

import java.util.Arrays;


@NoArgsConstructor
public final class LevelUPTask extends BaseTask {
    TroopOperation operation;
    private int[] staminaList;
    private boolean[] supplyList;
    int[] clearMarkList;
    int[] clearTabList;
    int waitingTime;
    public LevelUPTask(SimiSgz simiSgz, RobotAction robot, CoordinatesReader coordinatesReader, int index) {
        super(simiSgz, robot, coordinatesReader, index);
        this.staminaList=simiSgz.getStaminaList()[index];
        this.supplyList=simiSgz.getSupplyList()[index];
        this.clearMarkList=simiSgz.getClearMarkList()[index];
        this.clearTabList=simiSgz.getClearTabList()[index];
        this.waitingTime=simiSgz.getWaitingTimeList()[index];
    }

    @Override
    public void run() {
        int mainCityArmyNumber= simiSgz.getMainCityArmyNumber();
        int secondCityArmyNumber= simiSgz.getSecondCityArmyNumber();
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
                            operation.enterCity(i);
                            enteredCity=true;
                        }
                        //C. enter the second city.
                        if(!enteredSecondCity&&i>=mainCityArmyNumber){
                            operation.exitCity();
                            operation.scrollToBottom();
                            operation.enterCity(i);
                            enteredSecondCity=true;
                        }
                        operation.supplyArmy(mainCityArmyNumber, i);
                    }
                }
                //B. go to the mark button.
                operation.exitCity();
                operation.scrollToBottom();
            }

            //B. enter city and supply army
            for (int i = 0; i < totalArmyNumber; i++) {
                if(staminaList[i]<0)continue;
                //A。AWT operations
                operation.clear(mainCityArmyNumber,simiSgz.isAvoidMarchCollision(), i, clearMarkList[i], clearTabList[i]);
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
