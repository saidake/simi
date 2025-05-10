package com.simi.sgz;
import com.github.kwhat.jnativehook.keyboard.NativeKeyEvent;
import com.simi.sgz.AAAconfig.TaskType;
import com.simi.sgz.action.TaskService;
import com.simi.sgz.tasks.ExecutableTask;
import com.simi.sgz.utils.JNativeUtils;
import lombok.extern.slf4j.Slf4j;

import java.awt.*;
import java.util.Queue;

/**
 * A simple app designed for automating game operations like leveling up and attacking cities in the game
 * <a href="https://sangokushi.qookkagames.jp">Three Kingdoms Tactics<a/>.
 *
 * @author Craig Brown
 * @since 1.0
 */
@Slf4j
public class SimiSgzLevelUpApp {
    public static void main(String[] args) throws AWTException, IllegalAccessException, InstantiationException {
        // Prerequisites
        //!. All troops have returned to the city.
        //2. There are no enemy troops around the city.
        Queue<ExecutableTask> executableTasks =TaskService.createTaskQueue( TaskType.LEVEL_UP);
        // 30 45 60 75 90 105 120
        JNativeUtils.setupGlobalKeyEventListener(
                NativeKeyEvent.VC_P,
                ()-> System.exit(0),
                "Press 'P' to interrupt the application."
        );
        executableTasks.forEach(ExecutableTask::execute);
        //ThreadPoolContext.shutdown();
    }

}
