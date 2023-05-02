//package com.saidake.boot;
//
//import com.saidake.context.ConfigurableApplicationContext;
//import com.saidake.core.io.ResourceLoader;
//import com.saidake.util.StopWatch;
//
//import java.util.ArrayList;
//import java.util.List;
//
//public class SdkApplication {
//    private List<BootstrapRegistryInitializer> bootstrapRegistryInitializers;
//
//    public SdkApplication(Class... primarySources) {
//        this((ResourceLoader)null, primarySources);
//    }
//
//
//    public SdkApplication(ResourceLoader resourceLoader, Class... primarySources) {
//        this.bootstrapRegistryInitializers = this.getBootstrapRegistryInitializersFromSpringFactories();
//    }
//
//    /**
//     * Init run by parameters "...args", jump to ${@link SdkApplication#run(Class[], String[])}
//     * @param primarySource
//     * @param args
//     * @return
//     */
//    public static ConfigurableApplicationContext run(Class<?> primarySource, String... args) {
//        return run(new Class[]{primarySource}, args);
//    }
//
//    /**
//     * Init run by parameters "args[]", jump to ${@link SdkApplication#run(String[])}
//     * @param primarySources
//     * @param args
//     * @return
//     */
//    private static ConfigurableApplicationContext run(Class[] primarySources, String[] args) {
//        return (new SdkApplication(primarySources)).run(args);
//    }
//
//    /**
//     * Init run
//     * @param args
//     * @return
//     */
//    private ConfigurableApplicationContext run(String[] args) {
//        System.out.println("app started");
//        StopWatch stopWatch = new StopWatch();
//        stopWatch.start();
//        DefaultBootstrapContext bootstrapContext = this.createBootstrapContext();
//        return null;
//    }
//
//
////================================================================================================== tool method
//
//    private List<BootstrapRegistryInitializer> getBootstrapRegistryInitializersFromSpringFactories() {
//        ArrayList<BootstrapRegistryInitializer> initializers = new ArrayList();
//        this.getSpringFactoriesInstances(Bootstrapper.class).stream().map((bootstrapper) -> {
//            return bootstrapper::initialize;
//        }).forEach(initializers::add);
//        initializers.addAll(this.getSpringFactoriesInstances(BootstrapRegistryInitializer.class));
//        return initializers;
//    }
//
//    /**
//     * Create BootstrapContext
//     * @return
//     */
//    private DefaultBootstrapContext createBootstrapContext() {
//        DefaultBootstrapContext bootstrapContext = new DefaultBootstrapContext();
//        this.bootstrapRegistryInitializers.forEach((initializer) -> {
//            initializer.initialize(bootstrapContext);
//        });
//        return bootstrapContext;
//    }
//
//}
