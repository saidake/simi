package com.simi.sgz.AAAconfig;

import com.fasterxml.jackson.annotation.JsonCreator;

public enum TaskType {
    LEVEL_UP,DAILY,DEFENCE, ATTACK_CITY;
    @JsonCreator
    public static TaskType fromValue( String text){
        for( TaskType var : TaskType.values() ){
            if( var.name().replace("_","").equalsIgnoreCase(text) ){
                return var;
            }
        }
        return null;
    }
}
