package com.saidake.plugin.init.core.support.smyml;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Optional;

@Getter
@AllArgsConstructor
public enum WriteTypeEnum {
    APPEND_PROPERTIES_FOLDER("append-properties-folder"),
    APPEND_PROPERTIES("append-properties"),
    REPLACE_ALL("replace-all"),
    APPEND_STRING("append-string"),
    REPLACE_STRING("replace-string"),
    LINE_REPLACE("line-replace"),
    LINE_APPEND("line-append"),
    JAVA_ANNOTATION("java-annotation"),
    XML("xml");
    private final String value;

    public static Optional<WriteTypeEnum> fromValue(String value){
        for (WriteTypeEnum writeTypeEnum : values()) {
            if(writeTypeEnum.getValue().equals(value))return Optional.of(writeTypeEnum);
        }
        return Optional.empty();
    }
}
