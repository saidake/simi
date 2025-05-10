package com.simi.sgz.tasks;


import cn.hutool.setting.yaml.YamlUtil;
import com.simi.sgz.AAAconfig.SgzConstants;
import com.simi.sgz.action.RobotAction;
import com.simi.sgz.domain.properties.CoordinatesReader;
import com.simi.sgz.domain.properties.LevelUpProperties;
import com.simi.sgz.domain.properties.SimiSgz;
import lombok.extern.slf4j.Slf4j;
import net.sourceforge.tess4j.TesseractException;

import java.util.Arrays;
@Slf4j
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
        log.info("Create task for account: {}",index);
        this.staminaList=this.convertStaminaList(levelUpProperties.getStaminaList()[index]);
        log.info("Converted staminaList {} for account {}",this.staminaList,index);
        this.selectTroopFrom=levelUpProperties.getSelectTroopFrom()[index];
        this.supplyList=levelUpProperties.getSupplyList()[index];
        this.clearMarkList=levelUpProperties.getClearMarkList()[index];
        this.clearTabList=levelUpProperties.getClearTabList()[index];
        this.waitingTime=levelUpProperties.getWaitingTimeList()[index];
    }

    private int[] convertStaminaList(int[] staminaList) {
        boolean enteredCity=false;
        boolean enteredSecondCity=false;
        for (int i = 0; i < staminaList.length; i++) {
            if(staminaList[i]>0){
                if(!enteredCity){
                    this.operation.enterCity(i);
                    enteredCity=true;
                }
                // Enter the second city.
                if(!enteredSecondCity && i>= SgzConstants.CITY_TROOP_NUMBER){
                    this.operation.exitCity();
                    this.operation.scrollToBottom();
                    this.operation.enterCity(i);
                    enteredSecondCity=true;
                }
                try {
                    this.operation.initializeStamina(i, staminaList);
                } catch (TesseractException e) {
                    throw new RuntimeException(e);
                }
            }
        }
        // Exit City
        if(enteredCity)this.operation.exitCity();
        log.info("Final staminaList: {}", staminaList);
        return staminaList;
    }

    @Override
    public void loadProperties() {
        this.levelUpProperties= YamlUtil.loadByPath(SgzConstants.SGZ_LEVEL_UP_PATH, LevelUpProperties.class);
    }

    @Override
    public void executableTask() {
        //A. Check if there is a need to supply the army.
        //A.start the level-up action
        int positiveNum = (int) Arrays.stream(staminaList).filter(item -> item >= 15).count();
        while (positiveNum>0){
            boolean needSupplyForAny=false;
            //B. reduce stamina
            for (int i = 0; i < staminaList.length; i++) {
                if(staminaList[i]>=15){
                    staminaList[i]-=15;
                    if(supplyList[i])needSupplyForAny=true;
                    if(staminaList[i]<15){
                        positiveNum--;
                        staminaList[i]=0;
                    }
                }
            }
            //B. enter city and supply army
            if(needSupplyForAny){
                replenishTroops();
            }
            this.operation.scrollToBottom();

            // Execute tasks
            for (int i = 0; i < staminaList.length; i++) {
                if(staminaList[i]==0)continue;
                //A。AWT operations
                this.operation.clear(simiSgz.isAvoidMarchCollision(), selectTroopFrom[i], clearMarkList[i], clearTabList[i]);
            }
            try {
                int adjustedWaitingTime=waitingTime*1000 + SgzConstants.WRITING_TIME_OFFSET;
                Thread.sleep(adjustedWaitingTime+adjustedWaitingTime/2);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }
    }

    /**
     * Replenishing the troops.
     */
    private void replenishTroops() {
        boolean enteredCity=false;
        boolean enteredSecondCity=false;
        for (int i = 0; i < staminaList.length; i++) {
            if(supplyList[i]&&staminaList[i]>0){
                if(!enteredCity){
                    this.operation.enterCity(i);
                    enteredCity=true;
                }
                //C. enter the second city.
                if(!enteredSecondCity && i>= SgzConstants.CITY_TROOP_NUMBER){
                    this.operation.exitCity();
                    this.operation.scrollToBottom();
                    this.operation.enterCity(i);
                    enteredSecondCity=true;
                }
                this.operation.replenishTroop(i);
            }
        }
        //B. go to the mark button.
        if(enteredCity)this.operation.exitCity();
    }

}
