//package com.saidake.util;
//import com.saidake.lang.Nullable;
//
//public class StopWatch {
//    @Nullable
//    private String currentTaskName;
//    private long startTimeNanos;
//
//    public void start() throws IllegalStateException {
//        this.start("");
//    }
//
//    public void start(String taskName) throws IllegalStateException {
//        if (this.currentTaskName != null) {
//            throw new IllegalStateException("Can't start StopWatch: it's already running");
//        } else {
//            this.currentTaskName = taskName;
//            this.startTimeNanos = System.nanoTime();
//        }
//    }
//}
