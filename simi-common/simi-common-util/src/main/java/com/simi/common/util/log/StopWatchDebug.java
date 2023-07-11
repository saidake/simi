package com.simi.common.util.log;

import jakarta.annotation.Nullable;
import lombok.extern.slf4j.Slf4j;
import org.springframework.util.StopWatch;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.OpenOption;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;


/**
 * Extension for detecting the runtime of program fragments.
 *
 * @author Craig Brown
 * @since 1.0
 */
@Slf4j
public class StopWatchDebug {
    private static StopWatch STOP_WATCH;
    private static final String LOG_TITLE ="[STOP_WATCH_TASK: ";
    private static final String LOG_TITLE_START =" ] start=====================================";
    private static final String LOG_TITLE_END =  " ] end=======================================";
    /**
     * limits task Name.(e.g. "[STOP_WATCH_TASK: start    ] end=======================================")
     */
    private static final String TITLE_TASK_FORMAT ="%-13s";
    private static final String TITLE_DIVIDER ="----------------------------------------------------------------------------";

    //================================================================ tab bar string
    private static final String TOTAL_TIME_MILLIS_FORMAT="%9s";
    private static final String TOTAL_TIME_SECONDS_FORMAT="%9.3f";
    private static final String TASK_NAME_FORMAT="%-25s";
    private static final String TIME_MILLIS_FORMAT="%-15s";
    private static final String TIME_SECONDS_FORMAT="%-15.3f";
    private static final String TIME_PERCENT_FORMAT="%6.2f%%   ";

    /**
     * The format of taskName, millis and seconds is the same as title.
     * except that percentage is a different title format.
     */
    private static final String TITLE_TIME_PERCENT_FORMAT="%-10s";
    private static final String TITLE_TAB_BAR="|" + String.format(TASK_NAME_FORMAT,"TaskName")
            + "|"+String.format(TIME_MILLIS_FORMAT,"ms")
            + "|"+String.format(TIME_MILLIS_FORMAT,"s")
            + "|"+String.format(TITLE_TIME_PERCENT_FORMAT,"%")
            +"|";

    public static void init() {
        STOP_WATCH = new StopWatch("StopWatch");
    }
    public static void init(String watchId) {
        STOP_WATCH = new StopWatch(watchId);
    }

    public static String initAndStart(String taskName) {
        STOP_WATCH = new StopWatch("WATCH");
        return start(taskName);
    }

    public static String initAndStart(String watchId, String taskName) {
        STOP_WATCH = new StopWatch(watchId);
        return start(taskName);
    }
    public static String start(String taskName) {
        if(STOP_WATCH==null)STOP_WATCH = new StopWatch("StopWatch");
        STOP_WATCH.start(taskName);
        return LOG_TITLE + limitTaskName(taskName) + LOG_TITLE_START;
    }
    public static String restart(String taskName) {
        if(STOP_WATCH.isRunning())STOP_WATCH.stop();
        STOP_WATCH.start(taskName);
        return LOG_TITLE + limitTaskName(STOP_WATCH.getLastTaskName()) + LOG_TITLE_END + System.lineSeparator() +
                LOG_TITLE + limitTaskName(taskName)+ LOG_TITLE_START;
    }

    public static String stop() {
        STOP_WATCH.stop();
        return LOG_TITLE + limitTaskName(STOP_WATCH.getLastTaskName()) + LOG_TITLE_END;
    }

    private static String limitTaskName(String target) {
        return String.format(TITLE_TASK_FORMAT,target);
    }

    public static String printFile(String filepath) throws IOException {
        String print = print(false);
        Files.writeString(Path.of(filepath), System.lineSeparator()+print, StandardOpenOption.APPEND);
        return print;
    }
    public static String print(@Nullable boolean showLastTaskTitle) {
        if(STOP_WATCH.isRunning())STOP_WATCH.stop();
        long totalTimeMillis = STOP_WATCH.getTotalTimeMillis();
        double totalTimeSeconds = STOP_WATCH.getTotalTimeSeconds();
        String shortSummary = STOP_WATCH.getId() + ": total running time [ " + String.format(TOTAL_TIME_MILLIS_FORMAT, totalTimeMillis) + "ms / " + String.format(TOTAL_TIME_SECONDS_FORMAT, totalTimeSeconds) + "s ]";
        StringBuilder sb;
        if(showLastTaskTitle){
            sb=new StringBuilder(
                    LOG_TITLE + limitTaskName(STOP_WATCH.getLastTaskName()) + LOG_TITLE_END+ System.lineSeparator()
            );
        }else{
            sb=new StringBuilder();
        }
        sb.append(TITLE_DIVIDER).append(System.lineSeparator());
        sb.append(shortSummary);
        sb.append('\n');
        sb.append(TITLE_TAB_BAR).append(System.lineSeparator());
        for (StopWatch.TaskInfo task : STOP_WATCH.getTaskInfo()) {
            sb.append("|")
                    .append(String.format(TASK_NAME_FORMAT, task.getTaskName()));
            sb.append("|")
                    .append(String.format(TIME_MILLIS_FORMAT, task.getTimeMillis()));
            sb.append("|")
                    .append(String.format(TIME_SECONDS_FORMAT, task.getTimeSeconds()));
            sb.append("|")
                    .append(String.format(TIME_PERCENT_FORMAT, (double) task.getTimeMillis()*100 / totalTimeMillis))
                    .append("|");
            sb.append(System.lineSeparator());
        }
        sb.append(TITLE_DIVIDER).append(System.lineSeparator());
        return sb.toString();
    }
    public static String print() {
        return print(true);
    }
}