package com.simi.sgz.tasks;

import com.simi.sgz.action.RobotAction;
import com.simi.sgz.action.TroopOperation;
import com.simi.sgz.domain.properties.CoordinatesReader;
import com.simi.sgz.domain.properties.SimiSgz;
import lombok.NoArgsConstructor;

@NoArgsConstructor
public abstract class PropertyTask {
    protected SimiSgz simiSgz;
    protected TroopOperation operation;

    public PropertyTask(SimiSgz simiSgz, RobotAction robot, CoordinatesReader coordinatesReader, int index) {
        this.simiSgz=simiSgz;
        this.operation=new TroopOperation(robot, simiSgz, coordinatesReader.getCoordinates().get(index));
        this.initialize();
    }
    protected void initialize() {
        loadProperties();
    }
    public abstract void loadProperties();
}
