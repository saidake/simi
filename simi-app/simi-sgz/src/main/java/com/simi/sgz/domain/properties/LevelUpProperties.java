package com.simi.sgz.domain.properties;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class LevelUpProperties {
    private int minus;
    private int[][] staminaList;
    private boolean[][] supplyList;
    private int[][] clearMarkList;
    private int[][] clearTabList;
    private int[] waitingTimeList;
}
