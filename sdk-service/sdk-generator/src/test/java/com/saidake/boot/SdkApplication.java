package com.saidake.boot;

import com.saidake.context.ConfigurableApplicationContext;

public class SaidakeApplication {

    public SaidakeApplication(Class... primarySources) {
    }

    /**
     * static factory by ...args
     * @param primarySource
     * @param args
     * @return
     */
    public static ConfigurableApplicationContext run(Class<?> primarySource, String... args) {
        return run(new Class[]{primarySource}, args);
    }

    /**
     * static factory by args[]
     * @param primarySources
     * @param args
     * @return
     */
    private static ConfigurableApplicationContext run(Class[] primarySources, String[] args) {
        return (new SaidakeApplication(primarySources)).run(args);
    }

    private ConfigurableApplicationContext run(String[] args) {
        System.out.println("saidake started");
        return null;
    }

}
