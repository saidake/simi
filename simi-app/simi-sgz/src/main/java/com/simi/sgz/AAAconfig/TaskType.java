package com.simi.sgz.AAAconfig;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;

public enum TaskType {
    LEVELUP,DAILY;
    @JsonCreator
    public static TaskType fromValue( String text){
        for( TaskType var : TaskType.values() ){
            if( var.name().equalsIgnoreCase(text) ){
                return var;
            }
        }
        return null;
    }
}
