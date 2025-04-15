package com.simi.sandbox.temporal;

import io.temporal.workflow.WorkflowInterface;
import io.temporal.workflow.WorkflowMethod;

@WorkflowInterface
public interface SimiWorkflow {
    @WorkflowMethod
    void executeWorkflow(String input);
}
