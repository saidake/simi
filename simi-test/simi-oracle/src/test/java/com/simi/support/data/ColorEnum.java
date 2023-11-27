package com.simi.support.data;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Getter
@AllArgsConstructor
@NoArgsConstructor
@ToString
public enum ColorEnum {
    RED("a",23,33.3),
    BLUE("b",24,33.4),
    CYAN,
    GREEN("c",25,33.5);

    private String strVal;
    private int intVal;
    private double doubleVal;
}
