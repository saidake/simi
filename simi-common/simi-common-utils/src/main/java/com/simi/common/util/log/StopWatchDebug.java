package com.simi.common.util.log;
import jakarta.annotation.Nullable;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.util.Assert;
import org.springframework.util.StopWatch;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.MathContext;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.CompletableFuture;

/**
 * Extension for detecting the runtime of program fragments.
 *
 * @author Craig Brown
 * @since 1.0
 */
@Slf4j
public class StopWatchDebug {
    private static final DateTimeFormatter dateTimeFormatter= DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS");
    private static final Map<String,Integer> taskNameLevelMap =new HashMap<>();
    private static final Map<String, List<Long>> taskDurationMap =new HashMap<>();
    private static volatile StopWatch stopWatch;

    private static final Map<String, Map<Integer,Long>> asyncTaskTimesDurationMap =new HashMap<>();
    private static final Map<String, Integer> asyncTaskTimesMap =new HashMap<>();
    private static StopWatch asyncStopWatch;
    //============================================================================================ duration time suffix
    private static final String ASYNC_DURATION_START_SUFFIX="-async-start";
    private static final String ASYNC_DURATION_END_SUFFIX="-async-end";

    private static final String DURATION_START_SUFFIX="-start";
    private static final String DURATION_END_SUFFIX="-end";
    //============================================================================================== task log string
    private static final String LOG_TITLE ="[STOP_WATCH_TASK - ";
    private static final String LOG_TITLE_TIME_DIVIDER =" : ";
    private static final String LOG_TITLE_START =" ] start=====================================";
    private static final String LOG_TITLE_END =  " ] end=======================================";
    /**
     * limits task Name.(e.g. "[STOP_WATCH_TASK: start    ] end=======================================")
     */
    private static final String TITLE_TASK_FORMAT ="%-13s";
    private static final String TITLE_DIVIDER ="------------------------------------------------------------------------------------------------------------------------------------------";

    //====================================================================================== task content header format
    private static final String TOTAL_TIME_MILLIS_FORMAT="%9d";
    private static final String TOTAL_TIME_SECONDS_FORMAT="%9.3f";

    private static final String TASK_NAME_HEADER_FORMAT ="%-45s";
    private static final String TASK_LEVEL_HEADER_FORMAT ="%-6s";
    private static final String TASK_EXECUTION_NUMBER_HEADER_FORMAT ="%-6s";
    private static final String TIME_MILLIS_HEADER_FORMAT ="%-15s";
    private static final String TIME_SECONDS_HEADER_FORMAT ="%-15s";
    private static final String TIME_DURATION_MILLIS_HEADER_FORMAT ="%-15s";
    private static final String TIME_PERCENT_HEADER_FORMAT ="%-10s";
    private static final String TIME_DURATION_PERCENT_HEADER_FORMAT ="%-12s";
    //============================================================================================= task content format
    private static final String TASK_NAME_VALUE_FORMAT ="%-45s";
    private static final String TASK_LEVEL_VALUE_FORMAT ="%-6d";
    private static final String TASK_LEVEL_EMPTY_STRING=" ".repeat(6);
    private static final String TASK_EXECUTION_NUMBER_VALUE_FORMAT ="%-6d";
    private static final String TIME_MILLIS_VALUE_FORMAT ="%-15d";
    private static final String TIME_MILLIS_VALUE_EMPTY_STRING =" ".repeat(15);

    private static final String TIME_SECONDS_VALUE_FORMAT ="%-15.3f";
    private static final String TIME_SECONDS_VALUE_EMPTY_STRING =" ".repeat(15);

    private static final String TIME_DURATION_MILLIS_VALUE_FORMAT ="%-15d";
    private static final String TIME_DURATION_TIME_EMPTY_STRING=" ".repeat(15);
    private static final String TIME_PERCENT_VALUE_FORMAT ="%6.2f%%   ";
    private static final String TIME_PERCENT_EMPTY_STRING =" ".repeat(10);
    private static final String TIME_DURATION_PERCENT_VALUE_FORMAT ="%6.2f%%     ";
    private static final String TIME_DURATION_PERCENT_EMPTY_STRING=" ".repeat(12);


    //============================================================================================= task content header
    /**
     * The format of taskName, millis and seconds is the same as title.
     * except that percentage is a different title format.
     */
    private static final String TASK_CONTENT_HEADER =
            "|"+String.format(TASK_NAME_HEADER_FORMAT,"taskName")
                    + "|"+String.format(TASK_LEVEL_HEADER_FORMAT,"level")
                    + "|"+String.format(TASK_EXECUTION_NUMBER_HEADER_FORMAT,"times")
                    + "|"+String.format(TIME_MILLIS_HEADER_FORMAT,"ms")
                    + "|"+String.format(TIME_SECONDS_HEADER_FORMAT,"s")
                    + "|"+String.format(TIME_DURATION_MILLIS_HEADER_FORMAT,"duration(ms)")
                    + "|"+String.format(TIME_DURATION_PERCENT_HEADER_FORMAT,"duration(%)")
                    + "|"+String.format(TIME_PERCENT_HEADER_FORMAT,"%")
                    + "|";


    //====================================================================================== level content header format
    private static final String LEVEL_TASK_NAME_HEADER_FORMAT ="%-45s";
    private static final String LEVEL_LEVEL_HEADER_FORMAT ="%-6s";
    private static final String LEVEL_TIME_PERCENT_HEADER_FORMAT ="%-79s";

    //============================================================================================= level content format
    private static final String LEVEL_TASK_NAME_VALUE_FORMAT ="%-45s";
    private static final String LEVEL_LEVEL_VALUE_FORMAT ="%-6s";
    private static final Integer LEVEL_TIME_PERCENT_EMPTY_STRING_LENGTH=79;
    private static final String LEVEL_TIME_PERCENT_EMPTY_STRING=" ".repeat(LEVEL_TIME_PERCENT_EMPTY_STRING_LENGTH);


    //============================================================================================= level content header
    private static final String LEVEL_CONTENT_HEADER =
            "|"+String.format(LEVEL_TASK_NAME_HEADER_FORMAT,"taskName")
            +"|"+String.format(LEVEL_LEVEL_HEADER_FORMAT,"level")
            + "|"+String.format(LEVEL_TIME_PERCENT_HEADER_FORMAT,"percent")
            + "|";
    //================================================================================================== Start and stop
    private static void initializeStopWatchSingleton(String watchId){
        if(stopWatch==null){
            synchronized (StopWatchDebug.class){
                if(stopWatch==null){
                    stopWatch = new StopWatch(watchId);
                }
            }
        }
    }

    public static void init() {
        initializeStopWatchSingleton("WATCH");
    }
    public static void init(String watchId) {
        initializeStopWatchSingleton(watchId);
    }

    public static String initAndStart(String taskName) {
        initializeStopWatchSingleton("WATCH");
        return startAndGetTitle(taskName);
    }

    public static String initAndStart(String watchId, String taskName) {
        initializeStopWatchSingleton(watchId);
        return startAndGetTitle(taskName);
    }

    public static String startAndGetTitle(String taskName) {
        if(stopWatch ==null) initializeStopWatchSingleton("WATCH");
        setDurationMillis(taskName);
        stopWatch.start(taskName);
        return LOG_TITLE + dateTimeFormatter.format(LocalDateTime.now()) + LOG_TITLE_TIME_DIVIDER +
                limitTaskName(taskName) + LOG_TITLE_START;
    }
    public static String restart(String taskName) {
        if(stopWatch ==null) initializeStopWatchSingleton("WATCH");
        if(stopWatch.isRunning()) return stopAndGetTitle() + System.lineSeparator()+ startAndGetTitle(taskName);
        return startAndGetTitle(taskName);
    }
    public static String asyncRestart(String taskName) {
        String startString=null;
        String endString=null;
        if(asyncStopWatch ==null) asyncStopWatch = new StopWatch("AsyncStopWatch");
        if(asyncStopWatch.isRunning()){
            asyncStopWatch.stop();
            endString=LOG_TITLE + limitTaskName(asyncStopWatch.getLastTaskName()) + LOG_TITLE_END;
        }
        if(taskName.endsWith(ASYNC_DURATION_START_SUFFIX)||taskName.endsWith(ASYNC_DURATION_END_SUFFIX)) {
            asyncStopWatch.start(taskName);
            startString=LOG_TITLE + limitTaskName(taskName) + LOG_TITLE_START;
            String asyncDurationRealTaskName = getAsyncDurationRealTaskName(taskName);
            Integer times = asyncTaskTimesMap.get(asyncDurationRealTaskName);
            Map<Integer, Long> timesDurationMap = asyncTaskTimesDurationMap.computeIfAbsent(asyncDurationRealTaskName, key -> new LinkedHashMap<>());
            if(taskName.endsWith(ASYNC_DURATION_START_SUFFIX)){
                int timesAbs = times == null ? 1 : Math.abs(times) + 1;
                timesDurationMap.put(timesAbs,System.currentTimeMillis());
                asyncTaskTimesMap.put(asyncDurationRealTaskName, timesAbs);
            }
            if(taskName.endsWith(ASYNC_DURATION_END_SUFFIX)){
                Assert.isTrue(times!=null&&times>0,"asynchronous end times error");
                timesDurationMap.compute(times,(key,oldValue)->{
                    Assert.notNull(oldValue,"asynchronous end task error");
                    return System.currentTimeMillis()-oldValue;
                });
                asyncTaskTimesMap.put(asyncDurationRealTaskName,-times);
            }



        }
        return endString==null?startString: endString +System.lineSeparator()+ startString;
    }

    public static String stopAndGetTitle() {
        stop();
        return LOG_TITLE + dateTimeFormatter.format(LocalDateTime.now()) + LOG_TITLE_TIME_DIVIDER +
                limitTaskName(stopWatch.getLastTaskName()) + LOG_TITLE_END;
    }

    public static void stop() {
        if(stopWatch.isRunning())stopWatch.stop();
    }

    //===================================================================================================== Duration

    public static String initAndDurationStart(String taskName) {
        return initAndStart(taskName+DURATION_START_SUFFIX);
    }


    public static String initAndDuration(String taskName, Integer level) {
        Assert.isTrue(level>0,"level must be greater than or equal to 1");
        addTaskLevel(taskName,level);
        return initAndStart(taskName+DURATION_START_SUFFIX);
    }

    public static String duration(String taskName) {
        List<Long> longs = taskDurationMap.get(taskName);
        return restart(taskName+(longs==null||longs.size()%2==0?DURATION_START_SUFFIX:DURATION_END_SUFFIX));
    }
    public static String duration(String taskName, Integer level) {
        Assert.isTrue(level>0,"level must be greater than or equal to 1");
        addTaskLevel(taskName,level);
        List<Long> longs = taskDurationMap.get(taskName);
        return restart(taskName+(longs==null||longs.size()%2==0?DURATION_START_SUFFIX:DURATION_END_SUFFIX));
    }
    public static String asyncDuration(String taskName) {
        Integer times = asyncTaskTimesMap.get(taskName);
        return asyncRestart(taskName+(times==null||times<0?ASYNC_DURATION_START_SUFFIX:ASYNC_DURATION_END_SUFFIX));
    }

    public static String durationEndPrint(String taskName) {
        return restart(taskName+DURATION_END_SUFFIX)+System.lineSeparator()+print(null);
    }

    public static String durationEndPrint(String taskName, Integer level) {
        Assert.isTrue(level>0,"level must be greater than or equal to 1");
        addTaskLevel(taskName,level);
        return restart(taskName+DURATION_END_SUFFIX)+System.lineSeparator()+print(null);
    }
    public static String durationEndPrint(String taskName, Integer level, Integer limitLevel) {
        Assert.isTrue(level>0,"level must be greater than or equal to 1");
        addTaskLevel(taskName,level);
        return restart(taskName+DURATION_END_SUFFIX)+System.lineSeparator()+print(limitLevel);
    }
    //========================================================================================================= Print
    public static String printFile(String filepath) throws IOException {
        String print = print(null);
        Files.writeString(Path.of(filepath), System.lineSeparator()+print, StandardOpenOption.APPEND);
        return print;
    }
    public static String print(@Nullable Integer limitLevel) {
        if(stopWatch.isRunning())stop();
        if(asyncStopWatch!=null&&asyncStopWatch.isRunning())asyncStopWatch.stop();
        // Common data
        long totalTimeMillis = stopWatch.getTotalTimeMillis();
        double totalTimeSeconds = stopWatch.getTotalTimeSeconds();
        StringBuilder sb=new StringBuilder();
        // Print header string
        if(stopWatch.getTaskInfo().length>0){
            String taskEndString = stopAndGetTitle() + System.lineSeparator();
            String asyncTaskEndString = asyncStopWatch==null?"":LOG_TITLE +
                    limitTaskName(asyncStopWatch.getLastTaskName()) + LOG_TITLE_END + System.lineSeparator();
            sb.append(taskEndString).append(asyncTaskEndString);
        }
        sb.append(TITLE_DIVIDER).append(System.lineSeparator());
        String taskHeader = stopWatch.getId() +
                ": total running time [ " + String.format(TOTAL_TIME_MILLIS_FORMAT, totalTimeMillis) + "ms / " +
                String.format(TOTAL_TIME_SECONDS_FORMAT, totalTimeSeconds) + "s ]";
        sb.append(taskHeader);
        sb.append(System.lineSeparator());
        sb.append(TASK_CONTENT_HEADER).append(System.lineSeparator());
        // Define traversal common data
        Map<Integer, Long> parentDurationMap=new HashMap<>();
        Map<String,Integer> taskNameTimesMap =new HashMap<>();
        LinkedList<TaskInfo> parentTaskInfoList=new LinkedList<>();
        // Traverse taskInfo list
        for (StopWatch.TaskInfo taskInfo : stopWatch.getTaskInfo()) {
            String taskName = taskInfo.getTaskName();
            Integer times = taskNameTimesMap.merge(taskName, 1, (oldValue, value) -> ++oldValue);
            String durationRealTaskName = getDurationRealTaskName(taskName);
            Integer level = StopWatchDebug.taskNameLevelMap.get(durationRealTaskName);
            if(limitLevel!=null&&level!=null&&level>limitLevel)continue;
            Long durationByStart=null;
            Double currentLevelPercent=null;
            // Collect message level info
            if(taskName.endsWith(DURATION_START_SUFFIX)) {
                durationByStart=getDuration(durationRealTaskName, times);
                if(level!=null){
                    parentDurationMap.put(level,durationByStart);
                    Long parentDuration=parentDurationMap.get(level-1);
                    if(parentDuration!=null)currentLevelPercent=durationByStart.doubleValue()*100/parentDuration;
                }
                // Level statistics data
                if(level!=null&&(currentLevelPercent!=null||level==1)){
                    TaskInfo currentTaskInfo = new TaskInfo();
                    currentTaskInfo.setTaskName(durationRealTaskName);
                    currentTaskInfo.setLevel(level);
                    currentTaskInfo.setPercent(currentLevelPercent);
                    if(parentTaskInfoList.isEmpty()){
                        parentTaskInfoList.add(currentTaskInfo);
                    } else {
                        // Check parentTaskInfo
                        for (TaskInfo lastTaskInfo = parentTaskInfoList.getLast();;lastTaskInfo = parentTaskInfoList.getLast()){
                            if(lastTaskInfo==null)break;
                            if(lastTaskInfo.getLevel()<level){
                                lastTaskInfo.getChildTaskInfoList().add(currentTaskInfo);
                                parentTaskInfoList.addLast(currentTaskInfo);
                                break;
                            }
                            parentTaskInfoList.pollLast();
                            if(parentTaskInfoList.isEmpty())break;
                        }
                    }
                }
            }
            // Print Content string
            printSyncTaskStatisticHeader(taskInfo, sb, taskName, level, times, durationByStart, currentLevelPercent, totalTimeMillis);
        }


        Map<String,Integer> asyncTaskNameTimesMap =new HashMap<>();
        if(asyncStopWatch!=null){
            for (StopWatch.TaskInfo taskInfo : asyncStopWatch.getTaskInfo()) {
                String taskName = taskInfo.getTaskName();
                String durationRealTaskName = getAsyncDurationRealTaskName(taskName);
                Integer times = asyncTaskNameTimesMap.merge(taskName, 1, (oldValue, value) -> ++oldValue);
                Long durationByStart=null;
                if(taskName.endsWith(ASYNC_DURATION_START_SUFFIX)){
                    Map<Integer, Long> timesDurationMap = asyncTaskTimesDurationMap.get(durationRealTaskName);
                    Assert.notNull(timesDurationMap,"asynchronous millis null exception");
                    Long duration = timesDurationMap.get(times);
                    Assert.notNull(duration,"asynchronous duration null exception");
                    durationByStart=duration;
                }
                //B. content string
                printAsyncTaskStatisticHeader(sb, taskName, times, durationByStart);
            }
        }

        sb.append(TITLE_DIVIDER).append(System.lineSeparator());
        //B. level statistics string
        if(!parentTaskInfoList.isEmpty()){
            TaskInfo parentTaskInfo = parentTaskInfoList.getFirst();
            List<StringBuilder> levelStringList=new LinkedList<>();
            //C. header string
            buildLevelString(parentTaskInfo, levelStringList);
            Collections.reverse(levelStringList);
            for (StringBuilder levelString : levelStringList) {
                sb.append(levelString);
            }
        }
        clearSessionMap();
        return sb.toString();
    }

    private static void printAsyncTaskStatisticHeader(StringBuilder sb, String taskName, Integer times, Long durationByStart) {
        sb.append("|")
                .append(String.format(TASK_NAME_VALUE_FORMAT, taskName));
        sb.append("|")
                .append(TASK_LEVEL_EMPTY_STRING);
        sb.append("|")
                .append(String.format(TASK_EXECUTION_NUMBER_VALUE_FORMAT, times));
        sb.append("|")
                .append(TIME_MILLIS_VALUE_EMPTY_STRING);
        sb.append("|")
                .append(TIME_SECONDS_VALUE_EMPTY_STRING);
        sb.append("|")
                .append(durationByStart !=null?String.format(TIME_DURATION_MILLIS_VALUE_FORMAT, durationByStart):TIME_DURATION_TIME_EMPTY_STRING);
        sb.append("|")
                .append(TIME_DURATION_PERCENT_EMPTY_STRING);
        sb.append("|")
                .append(TIME_PERCENT_EMPTY_STRING)
                .append("|");
        sb.append(System.lineSeparator());
    }

    private static void printSyncTaskStatisticHeader(StopWatch.TaskInfo taskInfo, StringBuilder sb, String taskName,
                                                     Integer level, Integer times, Long durationByStart, Double currentLevelPercent,
                                                     long totalTimeMillis) {
        sb.append("|")
                .append(String.format(TASK_NAME_VALUE_FORMAT, taskName));
        sb.append("|")
                .append(level !=null?String.format(TASK_LEVEL_VALUE_FORMAT, level):TASK_LEVEL_EMPTY_STRING);
        sb.append("|")
                .append(String.format(TASK_EXECUTION_NUMBER_VALUE_FORMAT, times));
        sb.append("|")
                .append(String.format(TIME_MILLIS_VALUE_FORMAT, taskInfo.getTimeMillis()));
        sb.append("|")
                .append(String.format(TIME_SECONDS_VALUE_FORMAT, taskInfo.getTimeSeconds()));
        sb.append("|")
                .append(durationByStart !=null?String.format(TIME_DURATION_MILLIS_VALUE_FORMAT, durationByStart):TIME_DURATION_TIME_EMPTY_STRING);
        sb.append("|")
                .append(currentLevelPercent !=null?String.format(TIME_DURATION_PERCENT_VALUE_FORMAT, currentLevelPercent):TIME_DURATION_PERCENT_EMPTY_STRING);
        sb.append("|")
                .append(String.format(TIME_PERCENT_VALUE_FORMAT, (double) taskInfo.getTimeMillis()*100 / totalTimeMillis))
                .append("|");
        sb.append(System.lineSeparator());
    }

    private static void buildLevelString(TaskInfo parentTaskInfo, List<StringBuilder> levelStringList) {
        StringBuilder currentString=new StringBuilder();
        String levelHeader = "LEVEL STATISTICS: [ "+ parentTaskInfo.getTaskName()+", level "+ parentTaskInfo.getLevel()+" ]";
        currentString.append(levelHeader).append(System.lineSeparator());
        currentString.append(LEVEL_CONTENT_HEADER).append(System.lineSeparator());
        for (TaskInfo taskInfo : parentTaskInfo.getChildTaskInfoList()) {
            //C. content string
            currentString.append("|").append(String.format(LEVEL_TASK_NAME_VALUE_FORMAT, taskInfo.getTaskName()));
            currentString.append("|").append(String.format(LEVEL_LEVEL_VALUE_FORMAT, taskInfo.getLevel()));
            int repeatTimes = new BigDecimal(LEVEL_TIME_PERCENT_EMPTY_STRING_LENGTH, MathContext.UNLIMITED)
                    .multiply(BigDecimal.valueOf(taskInfo.getPercent())).divide(BigDecimal.valueOf(100)).intValue();
            if(LEVEL_TIME_PERCENT_EMPTY_STRING_LENGTH-repeatTimes>0)currentString.append("|").append("#".repeat(repeatTimes))
                    .append(" ".repeat(LEVEL_TIME_PERCENT_EMPTY_STRING_LENGTH-repeatTimes))
                    .append("|").append(System.lineSeparator());
            else currentString.append("|").append("ERROR").append(" ".repeat(LEVEL_TIME_PERCENT_EMPTY_STRING_LENGTH-5))
                    .append("|").append(System.lineSeparator());
            if(!taskInfo.getChildTaskInfoList().isEmpty())buildLevelString(taskInfo,levelStringList);
        }
        currentString.append(TITLE_DIVIDER).append(System.lineSeparator());
        levelStringList.add(currentString);
    }

    public static String print() {
        return print(null);
    }

    //===================================================================================================== Data Utils
    private static String getDurationRealTaskName(String taskName) {
        return durationRealTaskName(taskName, DURATION_START_SUFFIX, DURATION_END_SUFFIX);
    }
    private static String getAsyncDurationRealTaskName(String taskName) {
        return durationRealTaskName(taskName, ASYNC_DURATION_START_SUFFIX, ASYNC_DURATION_END_SUFFIX);
    }

    private static String durationRealTaskName(String taskName, String asyncDurationStartSuffix, String asyncDurationEndSuffix) {
        if(taskName.endsWith(asyncDurationStartSuffix))
            return taskName.substring(0, taskName.length() - asyncDurationStartSuffix.length());
        else if(taskName.endsWith(asyncDurationEndSuffix))
            return taskName.substring(0, taskName.length() - asyncDurationEndSuffix.length());
        else return taskName;
    }

    private static void addTaskLevel(String taskName, Integer level){
        taskNameLevelMap.put(taskName, level);
    }

    /**
     * it will be triggered when the duration start or end task started.
     * @param taskName  task name
     */
    private static void setDurationMillis(String taskName) {
        if(!taskName.endsWith(DURATION_START_SUFFIX)&&!taskName.endsWith(DURATION_END_SUFFIX)) return;
        String durationRealTaskName = getDurationRealTaskName(taskName);
        taskDurationMap.computeIfPresent(durationRealTaskName,(key,value)->{
            value.add(System.currentTimeMillis());
            return value;
        });
        taskDurationMap.computeIfAbsent(durationRealTaskName,key->{
            List<Long> millisList = new LinkedList<>();
            millisList.add(System.currentTimeMillis());
            return millisList;
        });
    }


    /**
     *
     * @param durationRealTaskName  duration real task name
     * @param times                 start from 1
     * @return  millis
     */
    private static Long getDuration(String durationRealTaskName, Integer times){
        List<Long> longList = taskDurationMap.get(durationRealTaskName);
        Assert.notNull(longList,"didn't find duration real task name");
        Assert.isTrue(longList.size()>=times<<1,"duration times error "+longList.size()+" "+times);
        Assert.isTrue(times>=1,"times must be greater than or equal to 1");
        return longList.get((times<<1)-1)-longList.get((times<<1)-2);
    }


    private static void clearSessionMap(){
        taskNameLevelMap.clear();
        taskDurationMap.clear();
        asyncTaskTimesMap.clear();
        asyncTaskTimesDurationMap.clear();
        asyncStopWatch=null;
        stopWatch=null;
    }
    private static String limitTaskName(String target) {
        return String.format(TITLE_TASK_FORMAT,target);
    }

    // Map<Integer, Map<String,List<Double>>> levelTaskNamePercentMap=new HashMap<>();
    @Data
    private static class TaskInfo{
        String taskName;
        Integer level;
        Double percent;
        List<TaskInfo> childTaskInfoList=new LinkedList<>();
    }

    //======================================================================================================= Data Utils

    private static void printTest() throws InterruptedException {
        System.out.println(StopWatchDebug.initAndStart("one"));
        System.out.println(StopWatchDebug.duration("test1",1));
        for (int i = 0; i < 100; i++) {
            Thread.sleep(10);
        }
        System.out.println(StopWatchDebug.duration("test1"));
        System.out.println(StopWatchDebug.duration("test2",2));
        for (int i = 0; i < 40; i++) {
            Thread.sleep(10);
        }
        System.out.println(StopWatchDebug.duration("test2",2));
        System.out.println(StopWatchDebug.duration("test21",2));
        for (int i = 0; i < 30; i++) {
            Thread.sleep(10);
        }
        System.out.println(StopWatchDebug.duration("test21",2));
        System.out.println(StopWatchDebug.duration("test3",3));
        for (int i = 0; i < 10; i++) {
            Thread.sleep(10);
        }
        CompletableFuture<Void> voidCompletableFuture = CompletableFuture.runAsync(() -> {
            System.out.println(StopWatchDebug.asyncDuration("async1"));
            for (int i = 0; i < 30; i++) {
                try {
                    Thread.sleep(20);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
            System.out.println(StopWatchDebug.asyncDuration("async1"));
            System.out.println(StopWatchDebug.asyncDuration("async1"));
            for (int i = 0; i < 30; i++) {
                try {
                    Thread.sleep(20);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
            System.out.println(StopWatchDebug.asyncDuration("async1"));
        });
        System.out.println(StopWatchDebug.duration("test3",3));
        System.out.println(StopWatchDebug.duration("test22",2));
        for (int i = 0; i < 30; i++) {
            Thread.sleep(10);
        }
        CompletableFuture<Void> voidCompletableFuture2 = CompletableFuture.runAsync(() -> {
            System.out.println(StopWatchDebug.asyncDuration("async2"));
            for (int i = 0; i < 30; i++) {
                try {
                    Thread.sleep(20);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
            System.out.println(StopWatchDebug.asyncDuration("async2"));
            System.out.println(StopWatchDebug.asyncDuration("async2"));
            for (int i = 0; i < 30; i++) {
                try {
                    Thread.sleep(20);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
            System.out.println(StopWatchDebug.asyncDuration("async2"));
        });
        System.out.println(StopWatchDebug.duration("test22",2));
        System.out.println(StopWatchDebug.duration("test33",3));
        for (int i = 0; i < 10; i++) {
            Thread.sleep(10);
        }
        System.out.println(StopWatchDebug.duration("test33",3));
        voidCompletableFuture.join();
        voidCompletableFuture2.join();
        System.out.println(StopWatchDebug.print());
    }

    public static void main(String[] args) throws InterruptedException {
        System.out.println(StopWatchDebug.initAndStart("test1"));
        Thread.sleep(2000);
        System.out.println(StopWatchDebug.restart("test2"));
        Thread.sleep(3000);
        System.out.println(StopWatchDebug.restart("test3"));
        Thread.sleep(4000);
        System.out.println(StopWatchDebug.print());
    }
}
