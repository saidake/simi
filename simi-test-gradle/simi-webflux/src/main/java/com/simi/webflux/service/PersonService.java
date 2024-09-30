package com.simi.webflux.service;

import com.simi.webflux.domain.Person;
import org.springframework.stereotype.Service;
import software.amazon.awssdk.enhanced.dynamodb.DynamoDbEnhancedClient;
import software.amazon.awssdk.enhanced.dynamodb.DynamoDbTable;
import software.amazon.awssdk.enhanced.dynamodb.TableSchema;
import software.amazon.awssdk.enhanced.dynamodb.model.QueryConditional;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class PersonService {

    private final DynamoDbEnhancedClient dynamoDbEnhancedClient;
    private final DynamoDbTable<Person> personTable;

    public PersonService(DynamoDbEnhancedClient dynamoDbEnhancedClient) {
        this.dynamoDbEnhancedClient = dynamoDbEnhancedClient;
        this.personTable = dynamoDbEnhancedClient.table("Person", TableSchema.fromBean(Person.class));
    }

    // Insert data
    public void addPerson(Person person) {
        personTable.putItem(person);
    }

    // Retrieve data
    public Person getPersonById(String personId) {
        return personTable.getItem(r -> r.key(k -> k.partitionValue(personId)));
    }

    // Query by condition (optional)
    public List<Person> queryPersonsByAge(int age) {
        return personTable.query(r -> r.queryConditional(QueryConditional.keyEqualTo(k -> k.partitionValue("age").sortValue(age))))
                .items()
                .stream()
                .collect(Collectors.toList());
    }
}
