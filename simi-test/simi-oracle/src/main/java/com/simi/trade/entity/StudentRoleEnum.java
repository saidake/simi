package com.simi.trade.entity;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum StudentRoleEnum {
    MONITOR("monitor"),
    LEADER("leader"),
    NORMAL("normal");
    private String value;
}
