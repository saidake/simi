package com.simi.webflux.service;

import com.simi.common.test.domain.template.SimiDbTestService;
import com.simi.webflux.domain.TestPerson;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import software.amazon.awssdk.enhanced.dynamodb.DynamoDbEnhancedClient;
import software.amazon.awssdk.enhanced.dynamodb.DynamoDbTable;
import software.amazon.awssdk.enhanced.dynamodb.Expression;
import software.amazon.awssdk.enhanced.dynamodb.model.*;


import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class TestPersonService implements SimiDbTestService {
    private final DynamoDbEnhancedClient dynamoDbEnhancedClient;
    private final DynamoDbTable<TestPerson> testPersonTable;
    public List<TestPerson> getAllTestPersons() {
        // This fetches all records in the table
        return testPersonTable.scan().items().stream().toList();
    }
    // Insert data
    public void addPerson(TestPerson testPerson) {
        testPersonTable.putItem(testPerson);
    }

    // Retrieve data
    public TestPerson getPersonById(String testPersonId) {
        return testPersonTable.getItem(r -> r.key(k -> k.partitionValue(testPersonId)));
    }

    // Query by condition (optional)
    public List<TestPerson> queryPersonsByAge(int age) {
        return testPersonTable.query(r -> r.queryConditional(QueryConditional.keyEqualTo(k -> k.partitionValue("age").sortValue(age))))
                .items()
                .stream()
                .collect(Collectors.toList());
    }


    public void updateTestPersons() {
        // Define a condition to only update if the record already exists
        Expression conditionExpression = Expression.builder()
                .expression("attribute_exists(testPersonId)")  // Ensures the record exists
                .build();
        // Create the update item request
        TransactUpdateItemEnhancedRequest<TestPerson> updateItemRequest1 = TransactUpdateItemEnhancedRequest.builder(TestPerson.class)
                .item(TestPerson.builder().testPersonId("001").name("test-david").build())
                .conditionExpression(conditionExpression)
                .build();
        TransactUpdateItemEnhancedRequest<TestPerson> updateItemRequest2 = TransactUpdateItemEnhancedRequest.builder(TestPerson.class)
                .item(TestPerson.builder().testPersonId("003").name("test-alice").build())
                .conditionExpression(conditionExpression)
                .build();
        // Create the transaction request
        TransactWriteItemsEnhancedRequest transactWriteItemsRequest = TransactWriteItemsEnhancedRequest.builder()
                .addUpdateItem(testPersonTable, updateItemRequest1)
                .addUpdateItem(testPersonTable, updateItemRequest2)
                .build();
        // Execute the transaction
        dynamoDbEnhancedClient.transactWriteItems(transactWriteItemsRequest);
    }
}
