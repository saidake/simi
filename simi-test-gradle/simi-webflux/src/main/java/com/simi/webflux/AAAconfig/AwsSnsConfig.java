package com.simi.webflux.AAAconfig;

import software.amazon.awssdk.auth.credentials.AwsBasicCredentials;
import software.amazon.awssdk.auth.credentials.StaticCredentialsProvider;
import software.amazon.awssdk.regions.Region;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import software.amazon.awssdk.services.sns.SnsClient;

import java.net.URI;

@Configuration
public class AwsSnsConfig {
    @Bean
    public SnsClient snsClient() {
        return SnsClient.builder()
                .credentialsProvider(StaticCredentialsProvider.create(
                        AwsBasicCredentials.create("test", "test")
                ))
                .endpointOverride(URI.create("http://192.168.127.128:4566")) // Local DynamoDB endpoint
                .region(Region.US_EAST_1) // Set your AWS region here
                .build();
    }
}
