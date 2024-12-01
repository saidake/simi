package com.simi.sandbox.temporal.AAAconfig;

import com.simi.sandbox.temporal.impl.SimiActivityImpl;
import com.simi.sandbox.temporal.impl.SimiWorkflowImpl;
import io.temporal.client.WorkflowClient;
import io.temporal.worker.Worker;
import io.temporal.worker.WorkerFactory;
import io.temporal.serviceclient.WorkflowServiceStubs;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

@Component
public class TemporalWorkerRunner implements CommandLineRunner {
    @Override
    public void run(String... args) {
        WorkflowServiceStubs service = WorkflowServiceStubs.newInstance();
        WorkflowClient client = WorkflowClient.newInstance(service);
        WorkerFactory factory = WorkerFactory.newInstance(client);
        Worker worker = factory.newWorker("my-task-queue");

        // Register workflow and activity implementations with the worker
        worker.registerWorkflowImplementationTypes(SimiWorkflowImpl.class);
        worker.registerActivitiesImplementations(new SimiActivityImpl());

        // Start the worker factory to listen for tasks
        factory.start();
        System.out.println("Temporal Worker started and listening for tasks...");
    }
}
