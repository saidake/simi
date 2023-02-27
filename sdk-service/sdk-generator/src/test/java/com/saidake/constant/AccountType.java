package com.saidake.constant;


import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum AccountType {
    SIMULATED("a",123),REAL("b",456);
    private String dd;
    private Integer kk;
}
