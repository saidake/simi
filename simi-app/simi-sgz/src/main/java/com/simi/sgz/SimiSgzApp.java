package com.simi.sgz;

import cn.hutool.core.lang.Assert;
import cn.hutool.setting.yaml.YamlUtil;
import com.simi.sgz.AAAconfig.SgzConstants;
import com.simi.sgz.AAAconfig.TaskType;
import com.simi.sgz.action.TaskFactory;
import com.simi.sgz.tasks.BaseTask;
import com.simi.sgz.action.RobotAction;
import com.simi.sgz.domain.properties.CoordinatesReader;
import com.simi.sgz.domain.properties.SimiSgz;
import com.simi.sgz.utils.JNativeUtils;
import com.simi.sgz.utils.SgzReader;
import lombok.extern.slf4j.Slf4j;

import java.awt.*;
import java.util.LinkedList;
import java.util.Queue;

@Slf4j
public class SimiSgzApp {
    public static void main(String[] args) throws AWTException, InterruptedException, IllegalAccessException, InstantiationException {
        Assert.isTrue(args.length==1,"The number of parameters must be 1.");
        TaskType taskType = TaskType.valueOf(args[0].toUpperCase());
        CoordinatesReader coordinatesReader = SgzReader.loadCoordinates();
        SimiSgz simiSgz = YamlUtil.loadByPath(SgzConstants.SGZ_TROOPS_PATH, SimiSgz.class);
        log.debug("simiSgz: {}",simiSgz);
        // Prerequisites
        //!. All troops have returned to the city.
        //2. There are no enemy troops around the city.
        RobotAction robot = new RobotAction();
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
        for (int i = 0; i < coordinatesReader.getCoordinates().size(); i++) {
            BaseTask baseTask=TaskFactory.createTask(taskType,simiSgz,robot,coordinatesReader,i);
            runningThreads.add(baseTask);
            baseTask.start();
        }
        JNativeUtils.setupGlobalKeyEventListener(runningThreads);
    }

}
