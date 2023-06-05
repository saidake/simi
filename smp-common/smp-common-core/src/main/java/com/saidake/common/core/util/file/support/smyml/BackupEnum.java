package com.saidake.common.core.util.file.support.smyml;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum BackupEnum {
    CURRENT("current");
    private String value;

    public static BackupEnum fromValue(String value){
        for (BackupEnum backupEnum : values()) {
            if(backupEnum.getValue().equals(value))return backupEnum;
        }
        return null;
    }
}
