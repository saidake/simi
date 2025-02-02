package com.simi.sandbox.temporal.impl;

import com.simi.sandbox.temporal.SimiActivity;
import com.simi.sandbox.temporal.SimiWorkflow;
import io.temporal.workflow.Workflow;
import io.temporal.activity.ActivityOptions;
import io.temporal.workflow.WorkflowMethod;

import java.time.Duration;

public class SimiWorkflowImpl implements SimiWorkflow {
    private final SimiActivity simiActivities = Workflow.newActivityStub(SimiActivity.class, ActivityOptions.newBuilder()
            .setStartToCloseTimeout(Duration.ofMinutes(1))
            .build());

    @WorkflowMethod
    @Override
    public void executeWorkflow(String input) {
        String result = simiActivities.performTask(input);
        System.out.println("Activity result: " + result);
    }
}
