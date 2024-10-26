package com.simi.sgz.AAAconfig;

import com.fasterxml.jackson.annotation.JsonCreator;

public enum TaskType {
    LEVELUP,DAILY,DEFENCE;
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
