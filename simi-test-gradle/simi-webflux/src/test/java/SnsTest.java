import com.simi.webflux.SimiWebFluxApp;
import com.simi.webflux.service.TestMessageProducerService;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

@Slf4j
@SpringBootTest(classes = SimiWebFluxApp.class) // This annotation will load the full application context
public class SnsTest {
    @Autowired
    TestMessageProducerService testMessageProducerService;

    @Test
    public void sendMessage(){
        testMessageProducerService.publishToTopic("craig-test-message","arn:aws:sns:us-east-1:000000000000:SimiTopic");
    }

}
