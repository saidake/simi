package com.simi.trade.entity;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum StudentRoleEnum {
    MONITOR("monitor"),
    LEADER("leader"),
    NORMAL("normal");
    private String value;


    @JsonCreator
    public static StudentRoleEnum fromValue( String text){
        for( StudentRoleEnum var : StudentRoleEnum.values() ){
            if( String.valueOf(var.value).equals(text) ){
                return var;
            }
        }
        return null;
    }

    @JsonValue
    public String toString(){
        return String.valueOf(value);
    }

}
