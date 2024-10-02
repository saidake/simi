package com.simi.webflux.service;

import com.simi.common.test.domain.template.SimiMessageProducerTestService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import software.amazon.awssdk.services.sns.SnsClient;
import software.amazon.awssdk.services.sns.model.PublishRequest;
import software.amazon.awssdk.services.sns.model.PublishResponse;

@Service
@RequiredArgsConstructor
public class TestMessageProducerService implements SimiMessageProducerTestService {
    private final SnsClient snsClient;
    public void publishToTopic(String message, String topicArn) {
        PublishRequest publishRequest = PublishRequest.builder()
                .message(message)
                .topicArn(topicArn)
                .build();
        PublishResponse publishResponse = snsClient.publish(publishRequest);
        System.out.println("Message ID: " + publishResponse.messageId());
    }
}
