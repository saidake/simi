package com.saidake.common.core.util.log;

import lombok.extern.slf4j.Slf4j;
import org.springframework.util.StopWatch;


/**
 * 检测程序片段运行时间拓展
 */
@Slf4j
public class StopWatchDebug {

    private static final String LOG_TITLE ="[STOP_WATCH_TASK: ";
    private static final String LOG_TITLE_START ="] start=====================================";
    private static final String LOG_TITLE_END =  "] end=======================================";
    private static final String LIMIT_TASK_NAME ="%-9s";

    private static StopWatch STOP_WATCH;

    public static void init() {
        STOP_WATCH = new StopWatch("StopWatch");
    }
    public static void init(String watchId) {
        STOP_WATCH = new StopWatch(watchId);
    }
    public static String start(String taskName) {
        if(STOP_WATCH==null)STOP_WATCH = new StopWatch("StopWatch");
        STOP_WATCH.start(taskName);
        return LOG_TITLE + limitString(taskName) + LOG_TITLE_START;
    }
    public static String restart(String taskName) {
        STOP_WATCH.stop();
        STOP_WATCH.start(taskName);
        return LOG_TITLE + limitString(STOP_WATCH.getLastTaskName()) + LOG_TITLE_END + System.lineSeparator() +
                LOG_TITLE + limitString(taskName)+ LOG_TITLE_START;
    }

    public static String stop() {
        STOP_WATCH.stop();
        return LOG_TITLE + limitString(STOP_WATCH.getLastTaskName()) + LOG_TITLE_END;
    }

    private static String limitString(String target) {
        return String.format(LIMIT_TASK_NAME,target);
    }

    public static String prettyPrint() {
        // 编写总结
        String initLog="";
        if(STOP_WATCH.isRunning()){
            STOP_WATCH.stop();
            initLog=LOG_TITLE + limitString(STOP_WATCH.getLastTaskName()) + LOG_TITLE_END+ System.lineSeparator();
        }
        // 获取运行的毫秒数与秒数
        long totalTimeMillis = STOP_WATCH.getTotalTimeMillis();
        double totalTimeSeconds = STOP_WATCH.getTotalTimeSeconds();
        String shortSummary = STOP_WATCH.getId() + ": total running time [ " + String.format("%9s", totalTimeMillis) + "ms / " + String.format("%9.3f", totalTimeSeconds) + "s ]";
        StringBuilder sb = new StringBuilder(initLog);
        sb.append("---------------------------------------------------------------").append(System.lineSeparator());
        sb.append(shortSummary);
        sb.append('\n');
        sb.append("|TaskName    |ms             |s              |%         |").append(System.lineSeparator());
        for (StopWatch.TaskInfo task : STOP_WATCH.getTaskInfo()) {
            sb.append("|")
                    .append(String.format("%-12s", task.getTaskName()));
            sb.append("|")
                    .append(String.format("%-15s", task.getTimeMillis()));
            sb.append("|")
                    .append(String.format("%-15.3f", task.getTimeSeconds()));
            sb.append("|")
                    .append(String.format("%6.2f%%   ", (double) task.getTimeMillis()*100 / totalTimeMillis))
                    .append("|");
            sb.append(System.lineSeparator());
        }
        sb.append("---------------------------------------------------------------").append(System.lineSeparator());
        return sb.toString();
    }
}