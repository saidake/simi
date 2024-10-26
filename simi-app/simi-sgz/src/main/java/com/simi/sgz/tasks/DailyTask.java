package com.simi.sgz.tasks;

import com.simi.sgz.action.RobotAction;
import com.simi.sgz.domain.properties.CoordinatesReader;
import com.simi.sgz.domain.properties.SimiSgz;


public final class DailyTask extends ThreadPoolTask  {
    public DailyTask(SimiSgz simiSgz, RobotAction robot, CoordinatesReader coordinatesReader, int index) {
        super(simiSgz, robot, coordinatesReader, index);
    }
    public void prepareExecutableTask() {
        operation.enterCity(0);
        operation.sendMessenger();
        operation.visit();
        operation.exitCity();
        operation.trials();
    }
}
