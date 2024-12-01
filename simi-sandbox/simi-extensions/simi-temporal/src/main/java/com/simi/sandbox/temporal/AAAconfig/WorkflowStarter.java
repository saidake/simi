package com.simi.sandbox.temporal.AAAconfig;

import com.simi.sandbox.temporal.SimiWorkflow;
import io.temporal.client.WorkflowClient;
import io.temporal.client.WorkflowOptions;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class WorkflowStarter {

    @Autowired
    private WorkflowClient workflowClient;

    public void startWorkflow(String input) {
        SimiWorkflow workflow = workflowClient.newWorkflowStub(SimiWorkflow.class,
                WorkflowOptions.newBuilder()
                        .setTaskQueue("my-task-queue")
                        .build());
        workflow.executeWorkflow(input);
    }
}
