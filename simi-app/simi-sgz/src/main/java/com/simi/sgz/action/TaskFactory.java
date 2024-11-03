package com.simi.sgz.action;

import com.simi.sgz.AAAconfig.TaskType;
import com.simi.sgz.domain.properties.CoordinatesReader;
import com.simi.sgz.domain.properties.SimiSgz;
import com.simi.sgz.tasks.*;

import java.util.Objects;

public class TaskFactory {
    public static ExecutableTask createTask(SimiSgz simiSgz, RobotAction robot, CoordinatesReader coordinatesReader, int index) {
        return switch (Objects.requireNonNull(TaskType.fromValue(simiSgz.getTaskType()))) {
            case LEVEL_UP -> new LevelUPTask(simiSgz, robot, coordinatesReader, index);
            case DAILY -> new DailyTask(simiSgz, robot, coordinatesReader, index);
            case DEFENCE -> new DefenceTask(simiSgz, robot, coordinatesReader, index);
            case ATTACK_CITY -> null;
        };
    }
}