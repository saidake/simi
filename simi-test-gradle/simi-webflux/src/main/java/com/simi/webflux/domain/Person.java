package com.simi.webflux.domain;
import software.amazon.awssdk.enhanced.dynamodb.mapper.annotations.DynamoDbBean;
import software.amazon.awssdk.enhanced.dynamodb.mapper.annotations.DynamoDbPartitionKey;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@DynamoDbBean
public class Person {
    private String personId;
    private String name;
    private int age;

    @DynamoDbPartitionKey
    public String getPersonId() {
        return personId;
    }

    public void setPersonId(String personId) {
        this.personId = personId;
    }

    // Other getters and setters...
}
