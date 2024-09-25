package com.simi.boot;


@FunctionalInterface
public interface BootstrapRegistryInitializer {
    void initialize(BootstrapRegistry registry);
}
