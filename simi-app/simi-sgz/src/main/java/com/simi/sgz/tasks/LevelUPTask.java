package com.simi.sgz.tasks;


import cn.hutool.setting.yaml.YamlUtil;
import com.simi.sgz.AAAconfig.SgzConstants;
import com.simi.sgz.action.RobotAction;
import com.simi.sgz.domain.properties.CoordinatesReader;
import com.simi.sgz.domain.properties.LevelUpProperties;
import com.simi.sgz.domain.properties.SimiSgz;

import java.util.Arrays;


public final class LevelUPTask extends ThreadPoolTask {
    private final int[] staminaList;
    private final int[] selectTroopFrom;
    private final boolean[] supplyList;
    private final int[] clearMarkList;
    private final int[] clearTabList;
    private final int waitingTime;

    private LevelUpProperties levelUpProperties;
    public LevelUPTask(SimiSgz simiSgz, RobotAction robot, CoordinatesReader coordinatesReader, int index) {
        super(simiSgz, robot, coordinatesReader, index);
        this.loadProperties();
        this.staminaList=levelUpProperties.getStaminaList()[index];
        this.selectTroopFrom=levelUpProperties.getSelectTroopFrom()[index];
        this.supplyList=levelUpProperties.getSupplyList()[index];
        this.clearMarkList=levelUpProperties.getClearMarkList()[index];
        this.clearTabList=levelUpProperties.getClearTabList()[index];
        this.waitingTime=levelUpProperties.getWaitingTimeList()[index];
    }

    @Override
    public void loadProperties() {
        LevelUpProperties levelUpProperties = YamlUtil.loadByPath(SgzConstants.SGZ_LEVEL_UP_PATH, LevelUpProperties.class);
        int[][] staminaList = levelUpProperties.getStaminaList();
        if(levelUpProperties.getMinus()!=0){
            for (int i = 0; i < staminaList.length; i++) {
                for (int j = 0; j < staminaList[0].length; j++) {
                    staminaList[i][j]-=levelUpProperties.getMinus();
                }
            }
        }
        this.levelUpProperties=levelUpProperties;
    }

    @Override
    public void executableTask() {
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
                            this.operation.enterCity(i);
                            enteredCity=true;
                        }
                        //C. enter the second city.
                        if(!enteredSecondCity&&i>=mainCityArmyNumber){
                            this.operation.exitCity();
                            this.operation.scrollToBottom();
                            this.operation.enterCity(i);
                            enteredSecondCity=true;
                        }
                        this.operation.supplyArmy(mainCityArmyNumber, i);
                    }
                }
                //B. go to the mark button.
                this.operation.exitCity();
                this.operation.scrollToBottom();
            }

            //B. enter city and supply army
            for (int i = 0; i < totalArmyNumber; i++) {
                if(staminaList[i]<0)continue;
                //A。AWT operations
                this.operation.clear(simiSgz.isAvoidMarchCollision(), selectTroopFrom[i], clearMarkList[i], clearTabList[i]);
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
