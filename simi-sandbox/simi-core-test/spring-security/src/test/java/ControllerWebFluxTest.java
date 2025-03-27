
import com.simi.aaa.spring.security.test.controller.TestControllerWebFlux;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.reactive.WebFluxTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.reactive.server.WebTestClient;
import reactor.core.publisher.Mono;

import static org.springframework.http.MediaType.APPLICATION_JSON;

@ExtendWith(SpringExtension.class)
@WebFluxTest(TestControllerWebFlux.class)
@Disabled
class ControllerWebFluxTest {

    @Autowired
    private WebTestClient webTestClient;

    @Test
    void testGet() {
        webTestClient.get()
                .uri(
                        uriBuilder ->
                        uriBuilder.path("/testGet").queryParam("id", 1).build())
                .exchange()
                .expectStatus().is5xxServerError();
    }

    @Test
    void testPost() {
        TestControllerWebFlux.TestRequestBody requestBody = new TestControllerWebFlux.TestRequestBody();
        requestBody.setName("test");
        requestBody.setDate(null);

        webTestClient.post()
                .uri("/testPost")
                .contentType(APPLICATION_JSON)
                .body(Mono.just(requestBody), TestControllerWebFlux.TestRequestBody.class)
                .exchange()
                .expectStatus().isOk()
                .expectBody(TestControllerWebFlux.TestRequestBody.class)
                .isEqualTo(requestBody);
    }

    @Test
    void testAsync() {
        webTestClient.get()
                .uri(uriBuilder -> uriBuilder.path("/testAsync").queryParam("id", 1).build())
                .header("Custom-Header", "test-value")
                .exchange()
                .expectStatus().isOk()
                .expectBody(String.class)
                .isEqualTo("success");
    }

    @Test
    void testTPP() {
        webTestClient.get()
                .uri(uriBuilder -> uriBuilder.path("/testTPP").queryParam("id", 1).build())
                .header("Authorization", "Bearer test-token")
                .exchange()
                .expectStatus().isOk()
                .expectBody(String.class)
                .isEqualTo("success");
    }
}