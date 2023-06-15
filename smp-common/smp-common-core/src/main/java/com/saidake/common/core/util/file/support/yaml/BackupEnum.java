package com.saidake.common.core.util.file.support.yaml;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Optional;

@Getter
@AllArgsConstructor
public enum BackupEnum {
    CURRENT("current"),
    SMP("smp");
    private final String value;

    public static Optional<BackupEnum> fromValue(String value){
        for (BackupEnum backupEnum : values()) {
            if(backupEnum.getValue().equals(value))return Optional.of(backupEnum);
        }
        return Optional.empty();
    }
}
