package com.simi.sgz.action;

import com.simi.sgz.AAAconfig.TaskType;
import com.simi.sgz.domain.properties.CoordinatesReader;
import com.simi.sgz.domain.properties.SimiSgz;
import com.simi.sgz.tasks.BaseTask;
import com.simi.sgz.tasks.DailyTask;
import com.simi.sgz.tasks.LevelUPTask;

public class TaskFactory {
    public static BaseTask createTask(TaskType taskType, SimiSgz simiSgz, RobotAction robot, CoordinatesReader coordinatesReader, int index) {
        return switch (taskType) {
            case LEVEL_UP -> new LevelUPTask(simiSgz, robot, coordinatesReader, index);
            case DAILY -> new DailyTask(simiSgz, robot, coordinatesReader, index);
        };
    }
}