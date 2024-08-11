package com.simi.sgz.domain;


import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public class Coordinate {
    private  int x ;
    private  int y;
    public static Coordinate of(int x, int y){
        return new Coordinate(x,y);
    }
}
