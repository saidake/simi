import com.simi.common.test.template.SimiDbTestUnit;
import com.simi.webflux.SimiWebFluxApp;
import com.simi.webflux.domain.TestPerson;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import software.amazon.awssdk.enhanced.dynamodb.DynamoDbEnhancedClient;
import software.amazon.awssdk.enhanced.dynamodb.DynamoDbTable;
import software.amazon.awssdk.enhanced.dynamodb.Expression;
import software.amazon.awssdk.enhanced.dynamodb.Key;
import software.amazon.awssdk.enhanced.dynamodb.model.DeleteItemEnhancedRequest;
import software.amazon.awssdk.enhanced.dynamodb.model.TransactUpdateItemEnhancedRequest;
import software.amazon.awssdk.enhanced.dynamodb.model.TransactWriteItemsEnhancedRequest;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertTrue;

//TODO: Log4j doesn't work
@Slf4j
@SpringBootTest(classes = SimiWebFluxApp.class) // This annotation will load the full application context
public class DynamoDbTest  implements SimiDbTestUnit {
    @Autowired
    private  DynamoDbEnhancedClient dynamoDbEnhancedClient;
    @Autowired
    private  DynamoDbTable<TestPerson> testPersonTable;

    @Override
    @Test
    public void addTest() {
        // Create a new TestPerson object
        TestPerson testPerson = TestPerson.builder()
                .testPersonId("003") // Make sure to set the partition key
                .name("Craig Brown")    // Assuming there's a 'name' field in TestPerson
                .age(39)             // Assuming there's an 'age' field in TestPerson
                .address("123 Elm Street") // Adding the 'address' field
                .build();
        testPersonTable.putItem(testPerson);
    }

    @Override
    @Test
    public void updateTest() {
        String testPersonId="004";
        // Retrieve the existing item from the DynamoDB table
        TestPerson existingPerson = testPersonTable.getItem(Key.builder().partitionValue(testPersonId).build());
        // Check if the item exists before attempting to update
        if (existingPerson != null) {
            // Update the item
            existingPerson = TestPerson.builder()
                    .testPersonId("003") // Make sure to set the partition key
                    .age(62)             // Assuming there's an 'age' field in TestPerson
                    .address("456 Elm Street") // Adding the 'address' field
                    .build();
            testPersonTable.updateItem(existingPerson);
        } else {
            //log.warn("TestPerson with ID '{}' does not exist. Update skipped.", testPersonId);
            System.out.println("Update skipped.");
        }
    }

    @Override
    @Test
    public void deleteTest() {
        // Delete only one record.
        Key key = Key.builder().partitionValue("004").build();
        testPersonTable.deleteItem(DeleteItemEnhancedRequest.builder().key(key).build());
//        // Delete multiple records according to specific fields.
//        Expression filterExpression = Expression.builder()
//                .expression("#name = :nameValue")
//                .putExpressionName("#name", "name")
//                .putExpressionValue(":nameValue", AttributeValue.builder().s("test-craig").build())
//                .build();
//        ScanEnhancedRequest scanRequest = ScanEnhancedRequest.builder()
//                .filterExpression(filterExpression)
//                .build();
//        // Loop through the results and delete each item
//        for (TestPerson person : testPersonTable.scan(scanRequest).items()) {
//            // Create the delete request for each person
//            Key key = Key.builder().partitionValue(person.getTestPersonId()).build();
//            testPersonTable.deleteItem(DeleteItemEnhancedRequest.builder().key(key).build());
//        }
    }

    @Override
    @Test
    public void queryTest() {
        List<TestPerson> allTestPersons = testPersonTable.scan().items().stream().toList();
        assertTrue(allTestPersons.size() > 0); // Ensure at least one record is fetched
        System.out.println(allTestPersons);
    }

    @Override
    @Test
    public void transactionTest() {
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
