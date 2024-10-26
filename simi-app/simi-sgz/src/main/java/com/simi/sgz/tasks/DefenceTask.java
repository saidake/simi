package com.simi.sgz.tasks;

import com.github.kwhat.jnativehook.keyboard.NativeKeyEvent;
import com.simi.sgz.action.RobotAction;
import com.simi.sgz.domain.properties.CoordinatesReader;
import com.simi.sgz.domain.properties.SimiSgz;
import com.simi.sgz.utils.JNativeUtils;
import lombok.NoArgsConstructor;

@NoArgsConstructor
public final class DefenceTask extends PropertyTask implements ExecutableTask {
    public DefenceTask(SimiSgz simiSgz, RobotAction robot, CoordinatesReader coordinatesReader, int index) {
        super(simiSgz, robot, coordinatesReader, index);
    }
    @Override
    public void execute() {
        JNativeUtils.setupGlobalKeyEventListener(NativeKeyEvent.VC_P,()-> System.exit(0),"");
    }
}
