import com.simi.common.test.template.SimiMessageProducerTestUnit;
import com.simi.common.util.data.RandomUtil;
import com.simi.webflux.SimiWebFluxApp;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import software.amazon.awssdk.services.sns.SnsClient;
import software.amazon.awssdk.services.sns.model.PublishRequest;
import software.amazon.awssdk.services.sns.model.PublishResponse;

@Slf4j
@SpringBootTest(classes = SimiWebFluxApp.class) // This annotation will load the full application context
@Disabled
public class SnsTest implements SimiMessageProducerTestUnit {

    @Autowired
    private SnsClient snsClient;
    @Test
    public void sendMessage(){
        String message="test-message-"+ RandomUtil.getRandomEnglishName();
        String topicArn="arn:aws:sns:us-east-1:000000000000:SimiTopic";
        PublishRequest publishRequest = PublishRequest.builder()
                .message(message)
                .topicArn(topicArn)
                .build();
        PublishResponse publishResponse = snsClient.publish(publishRequest);
        System.out.println("Message ID: " + publishResponse.messageId());
        // Sqs ID uniquely identifies the message in the SQS queue, and it's separate from the SNS message ID.
    }

}
