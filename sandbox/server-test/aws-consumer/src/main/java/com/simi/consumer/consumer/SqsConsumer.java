package com.simi.consumer.consumer;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import software.amazon.awssdk.services.sqs.SqsClient;
import software.amazon.awssdk.services.sqs.model.ReceiveMessageRequest;
import software.amazon.awssdk.services.sqs.model.Message;
import software.amazon.awssdk.services.sqs.model.DeleteMessageRequest;

import java.util.List;

@Component
@EnableScheduling
public class SqsConsumer {

    @Autowired
    private SqsClient sqsClient;

    @Value("${cloud.aws.sqs.queueUrl}") // Assuming you have set this in your application properties
    private String queueUrl;

    @Scheduled(fixedDelay = 5000) // Poll every 5 seconds
    public void pollQueue() {
        List<Message> messages = sqsClient.receiveMessage(ReceiveMessageRequest.builder()
                .queueUrl(queueUrl)
                .waitTimeSeconds(20) // Long polling
                .build())
                .messages();

        for (Message message : messages) {
            processMessage(message);
            deleteMessage(message);
        }
    }

    private void processMessage(Message message) {
        // Handle the received message
        System.out.println("Received message: " +message.messageId()+ " "+ message.body());
    }

    private void deleteMessage(Message message) {
        sqsClient.deleteMessage(DeleteMessageRequest.builder()
                .queueUrl(queueUrl)
                .receiptHandle(message.receiptHandle())
                .build());
    }
}
