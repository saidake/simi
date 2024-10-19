package com.simi.sgz.domain.properties;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class Coordinate {
    private String name;
    private int[] redPoint;
    private int[] stopBtn1;
    private int[] confirm;
    private int[] dangerousConfirm;
    private int[] cityAddTroops;
    private int[] cityConfirm;
    private int[] cityBack;
    private int[] armyBtn;
    private int[] coordinateBtn;
    private int[] scrollTop;
    private int[] scrollBot;
    private List<int[]> btns;
    private List<int[]> tabs;
    private List<int[]> armies;
    private List<int[]> marks;
}


