import com.simi.webflux.SimiWebFluxApp;
import com.simi.webflux.domain.TestPerson;
import com.simi.webflux.service.TestPersonService;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import software.amazon.awssdk.enhanced.dynamodb.DynamoDbEnhancedClient;
import software.amazon.awssdk.enhanced.dynamodb.DynamoDbTable;
import software.amazon.awssdk.enhanced.dynamodb.Expression;
import software.amazon.awssdk.enhanced.dynamodb.Key;
import software.amazon.awssdk.enhanced.dynamodb.model.DeleteItemEnhancedRequest;
import software.amazon.awssdk.enhanced.dynamodb.model.ScanEnhancedRequest;
import software.amazon.awssdk.services.dynamodb.model.AttributeValue;

import java.util.Iterator;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertTrue;

@Slf4j
@SpringBootTest(classes = SimiWebFluxApp.class) // This annotation will load the full application context
public class DynamoDbTest {
    @Autowired
    private TestPersonService testPersonService; // Inject the actual service
    @Autowired
    private  DynamoDbEnhancedClient dynamoDbEnhancedClient;
    @Autowired
    private  DynamoDbTable<TestPerson> testPersonTable;
    @Test
    public void dynamoDbGetTest() {
        List<TestPerson> allTestPersons = testPersonService.getAllTestPersons();
        assertTrue(allTestPersons.size() > 0); // Ensure at least one record is fetched
        System.out.println(allTestPersons);
    }
    @Test
    public void dynamoDbDeleteTest() {
        // Scan the table for items with the specified name
        Expression filterExpression = Expression.builder()
                .expression("#name = :nameValue")
                .putExpressionName("#name", "name")
                .putExpressionValue(":nameValue", AttributeValue.builder().s("test-craig").build())
                .build();
        ScanEnhancedRequest scanRequest = ScanEnhancedRequest.builder()
                .filterExpression(filterExpression)
                .build();
        // Scan and collect the results
        Iterator<TestPerson> results = testPersonTable.scan(scanRequest).items().iterator();

        // Loop through the results and delete each item
        while (results.hasNext()) {
            TestPerson person = results.next();
            // Create the delete request for each person
            Key key = Key.builder().partitionValue(person.getTestPersonId()).build();
            testPersonTable.deleteItem(DeleteItemEnhancedRequest.builder().key(key).build());
        }
    }
    @Test
    public void dynamoDbTransactionTest() {
        testPersonService.updateTestPersons();
    }
}
