package com.saidake.boot;


@FunctionalInterface
public interface BootstrapRegistryInitializer {
    void initialize(BootstrapRegistry registry);
}
