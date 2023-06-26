package com.saidake.common.util.file.support;
import java.util.Optional;

@FunctionalInterface
public interface InitCallback {
    Optional<String> check() throws InitException;
}
