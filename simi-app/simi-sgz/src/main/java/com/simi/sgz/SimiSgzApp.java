package com.simi.sgz;

import cn.hutool.setting.yaml.YamlUtil;
import com.github.kwhat.jnativehook.keyboard.NativeKeyEvent;
import com.simi.sgz.AAAconfig.SgzConstants;
import com.simi.sgz.action.TaskFactory;
import com.simi.sgz.tasks.ExecutableTask;
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
    public static void main(String[] args) throws AWTException, IllegalAccessException, InstantiationException {
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
        Queue<ExecutableTask> executableTasks =new LinkedList<>();
        // 30 45 60 75 90 105 120
        for (int i = 0; i < coordinatesReader.getCoordinates().size(); i++) {
            ExecutableTask executableTask =TaskFactory.createTask(simiSgz,robot,coordinatesReader,i);
            executableTasks.add(executableTask);
        }
        JNativeUtils.setupGlobalKeyEventListener(
                NativeKeyEvent.VC_P,
                ()-> System.exit(0),
                "Press 'P' to interrupt the application."
        );
        executableTasks.forEach(ExecutableTask::execute);
    }

}
