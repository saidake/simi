package com.simi.webflux.AAAconfig;

import com.simi.webflux.domain.TestPerson;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import software.amazon.awssdk.auth.credentials.AwsBasicCredentials;
import software.amazon.awssdk.auth.credentials.DefaultCredentialsProvider;
import software.amazon.awssdk.auth.credentials.StaticCredentialsProvider;
import software.amazon.awssdk.enhanced.dynamodb.DynamoDbEnhancedClient;
import software.amazon.awssdk.enhanced.dynamodb.DynamoDbTable;
import software.amazon.awssdk.enhanced.dynamodb.TableSchema;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.dynamodb.DynamoDbClient;

import java.net.URI;

@Configuration
public class DynamoDbConfig {
    @Bean
    public DynamoDbEnhancedClient DynamoDbEnhancedClient() {
        DynamoDbClient dynamoDbClient = DynamoDbClient.builder()
                .credentialsProvider(StaticCredentialsProvider.create(AwsBasicCredentials.create("test", "test")))
                .region(Region.US_EAST_1) // Specify your desired region here
                .endpointOverride(URI.create("http://192.168.127.128:4566")) // Local DynamoDB endpoint
                .build();
        return DynamoDbEnhancedClient.builder()
                .dynamoDbClient(dynamoDbClient)
                .build();
    }
    @Bean
    public DynamoDbTable<TestPerson> testPersonTable(DynamoDbEnhancedClient dynamoDbEnhancedClient){
        return dynamoDbEnhancedClient.table("TestPerson", TableSchema.fromBean(TestPerson.class));
    }

}
