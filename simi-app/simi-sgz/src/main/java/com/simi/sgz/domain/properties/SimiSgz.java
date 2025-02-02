package com.simi.sgz.domain.properties;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class SimiSgz {
    private String taskType;
    private int mainCityArmyNumber;
    private int secondCityArmyNumber;
    private boolean avoidMarchCollision;
}
