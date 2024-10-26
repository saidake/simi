package com.simi.sgz.tasks;

import com.simi.sgz.AAAconfig.ThreadPoolContext;
import com.simi.sgz.action.RobotAction;
import com.simi.sgz.domain.properties.CoordinatesReader;
import com.simi.sgz.domain.properties.SimiSgz;

import java.util.concurrent.ThreadPoolExecutor;

public abstract class ThreadPoolTask extends PropertyTask implements ExecutableTask  {
    public ThreadPoolTask(SimiSgz simiSgz, RobotAction robot, CoordinatesReader coordinatesReader, int index) {
        super(simiSgz, robot, coordinatesReader, index);
    }
    public abstract void prepareExecutableTask();
    @Override
    public void execute() {
        ThreadPoolExecutor threadPoolExecutor = ThreadPoolContext.getThreadPoolExecutor();
        threadPoolExecutor.execute(this::prepareExecutableTask);
    }
}
