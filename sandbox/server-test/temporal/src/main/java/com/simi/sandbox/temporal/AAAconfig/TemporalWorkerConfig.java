package com.simi.sandbox.temporal.AAAconfig;

import com.simi.sandbox.temporal.impl.SimiActivityImpl;
import com.simi.sandbox.temporal.impl.SimiWorkflowImpl;
import io.temporal.serviceclient.WorkflowServiceStubsOptions;
import io.temporal.worker.WorkerFactory;
import io.temporal.worker.Worker;
import io.temporal.client.WorkflowClient;
import io.temporal.serviceclient.WorkflowServiceStubs;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class TemporalWorkerConfig {

    @Bean
    public WorkflowServiceStubs workflowServiceStubs() {
        return WorkflowServiceStubs.newInstance(
        WorkflowServiceStubsOptions.newBuilder()
              .setTarget("127.0.0.1:7233") // Replace with your Temporal server address
              .build()
        ); // Connect to the Temporal server
    }

    @Bean
    public WorkflowClient workflowClient(WorkflowServiceStubs serviceStubs) {
        return WorkflowClient.newInstance(serviceStubs);
    }

    @Bean
    public WorkerFactory workerFactory(WorkflowClient workflowClient) {
        return WorkerFactory.newInstance(workflowClient);
    }

    @Bean
    public Worker workflowWorker(WorkerFactory workerFactory) {
        Worker worker = workerFactory.newWorker("SimiTestQueue"); // Specify the task queue
        worker.registerWorkflowImplementationTypes(SimiWorkflowImpl.class); // Register workflows
        worker.registerActivitiesImplementations(new SimiActivityImpl()); // Register activities
        workerFactory.start();
        return worker;
    }
}
