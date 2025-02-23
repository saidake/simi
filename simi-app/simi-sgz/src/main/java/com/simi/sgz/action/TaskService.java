package com.simi.sgz.action;

import cn.hutool.setting.yaml.YamlUtil;
import com.simi.sgz.AAAconfig.SgzConstants;
import com.simi.sgz.AAAconfig.TaskType;
import com.simi.sgz.AAAconfig.ThreadPoolContext;
import com.simi.sgz.domain.properties.CoordinatesReader;
import com.simi.sgz.domain.properties.SimiSgz;
import com.simi.sgz.tasks.*;
import com.simi.sgz.utils.PropertiesLoader;
import lombok.extern.slf4j.Slf4j;

import java.awt.*;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.Queue;
import java.util.concurrent.CompletableFuture;

/**
 * A factory class used to create SGZ tasks.
 *
 * @author Craig Brown
 * @since 1.0
 */
@Slf4j
public class TaskService {

    public static Queue<ExecutableTask> createTaskQueue(TaskType taskType) throws IllegalAccessException, InstantiationException, AWTException {
        CoordinatesReader coordinatesReader = PropertiesLoader.loadCoordinates();
        SimiSgz simiSgz = YamlUtil.loadByPath(SgzConstants.SGZ_TROOPS_PATH, SimiSgz.class);
        log.debug("simiSgz: {}",simiSgz);
        // Prerequisites
        //!. All troops have returned to the city.
        //2. There are no enemy troops around the city.
        RobotAction robot = new RobotAction();
        // 30 45 60 75 90 105 120
        List<CompletableFuture<ExecutableTask>> futures = new LinkedList<>();
        for (int i = 0; i < coordinatesReader.getCoordinates().size(); i++) {
            int finalI = i;
            CompletableFuture<ExecutableTask> future = CompletableFuture.supplyAsync(() -> {
                return TaskService.createTask( taskType, simiSgz,robot,coordinatesReader, finalI);
            }, ThreadPoolContext.getThreadPool());
            futures.add(future);
        }
        return CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).thenApply(v -> {
            Queue<ExecutableTask> results = new LinkedList<>();
            for (CompletableFuture<ExecutableTask> future : futures) {
                results.add(future.join()); // Get the result from each CompletableFuture
            }
            return results; // Return the merged results
        }).join();
    }

    /**
     * Create a single task.
     *
     * @param taskType  Task type
     * @param simiSgz   Core property object
     * @param robot     AWT object
     * @param coordinatesReader Coordinate Reader
     * @param index     The current account index
     * @return A single task.
     */
    private static ExecutableTask createTask(TaskType taskType, SimiSgz simiSgz, RobotAction robot, CoordinatesReader coordinatesReader, int index) {
        return switch (Objects.requireNonNull(taskType)) {
            case LEVEL_UP -> new LevelUPTask(simiSgz, robot, coordinatesReader, index);
            case DAILY -> new DailyTask(simiSgz, robot, coordinatesReader, index);
            case DEFENCE -> new DefenceTask(simiSgz, robot, coordinatesReader, index);
            case ATTACK_CITY -> null;
            case DISPATCH -> new DispatchTask(simiSgz,robot,coordinatesReader,index);
        };
    }
}