package com.simi.sgz;

import cn.hutool.setting.yaml.YamlUtil;
import com.simi.sgz.AAAconfig.LevelUPTask;
import com.simi.sgz.domain.*;
import com.simi.sgz.domain.properties.CoordinatesReader;
import com.simi.sgz.domain.properties.SimiSgz;
import lombok.extern.slf4j.Slf4j;

import java.awt.*;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

@Slf4j
public class LevelUP {
    public static void main(String[] args) throws AWTException, InterruptedException {
        CoordinatesReader coordinatesReader = YamlUtil.loadByPath("C:\\Users\\simi\\Desktop\\DevProjects\\simi\\AAA\\.simi\\simi-sgz\\simi-sgz-coordinates.yml", CoordinatesReader.class);
        log.debug("coordinatesReader: {}",coordinatesReader);
        SimiSgz simiSgz = YamlUtil.loadByPath("C:\\Users\\simi\\Desktop\\DevProjects\\simi\\AAA\\.simi\\simi-sgz\\simi-sgz.yml", SimiSgz.class);
        log.debug("simiSgz: {}",simiSgz);
        // Prerequisites
        //!. All troops have returned to the city.
        //2. There are no enemy troops around the city.
        // 15  30  45  60  75  90  105  120
        RobotAction robot = new RobotAction();
        List<Operation> operationList = Arrays.asList(Pineapple.builder().build(), Orange.builder().build(), Durian.builder().build(), Grape.builder().build());
        int[][] staminaList = simiSgz.getStaminaList();
        if(simiSgz.getMinus()!=0){
            for (int i = 0; i < staminaList.length; i++) {
                for (int j = 0; j < staminaList[0].length; j++) {
                    staminaList[i][j]-=simiSgz.getMinus();
                }
            }
        }
        Queue<Thread> runningThreads=new LinkedList<>();
        // 30 45 60 75 90 105 120
        for (int i = 0; i < 4; i++) {
            LevelUPTask levelUPTask = new LevelUPTask(
                    simiSgz.getMainCityArmyNumber(),
                    simiSgz.getSecondCityArmyNumber(),
                    robot,
                    operationList.get(i),
                    staminaList[i],
                    simiSgz.getSupplyList()[i],
                    simiSgz.getClearMarkList()[i],
                    simiSgz.getClearTabList()[i],
                    simiSgz.getWaitingTimeList()[i]
            );
            runningThreads.add(levelUPTask);
            levelUPTask.start();
        }
        JNativeUtils.setupGlobalKeyEventListener(runningThreads);
    }
}
