package com.simi.webflux.domain;
import lombok.*;
import software.amazon.awssdk.enhanced.dynamodb.mapper.annotations.DynamoDbAttribute;
import software.amazon.awssdk.enhanced.dynamodb.mapper.annotations.DynamoDbBean;
import software.amazon.awssdk.enhanced.dynamodb.mapper.annotations.DynamoDbPartitionKey;


@DynamoDbBean
@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
public class TestPerson {
    private String testPersonId;
    private String name;
    private int age;
    private String address;

    @DynamoDbPartitionKey
    public String getTestPersonId() {
        return this.testPersonId;
    }
}
