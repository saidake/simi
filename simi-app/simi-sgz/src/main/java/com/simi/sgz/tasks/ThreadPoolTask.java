package com.simi.sgz.tasks;

import com.simi.sgz.AAAconfig.ThreadPoolContext;
import com.simi.sgz.action.RobotAction;
import com.simi.sgz.domain.properties.CoordinatesReader;
import com.simi.sgz.domain.properties.SimiSgz;

/**
 * An abstract task class for executing tasks with a singleton thread pool instance.
 *
 * @author Craig Brown
 * @since 1.0
 */
public abstract class ThreadPoolTask extends PropertyTask implements ExecutableTask  {
    public ThreadPoolTask(SimiSgz simiSgz, RobotAction robot, CoordinatesReader coordinatesReader, int index) {
        super(simiSgz, robot, coordinatesReader, index);
    }
    public abstract void executableTask();

    @Override
    public void execute() {
        ThreadPoolContext.executeTask(this::executableTask);
    }
}
