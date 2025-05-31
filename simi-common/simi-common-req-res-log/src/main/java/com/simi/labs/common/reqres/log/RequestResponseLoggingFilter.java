package com.simi.labs.common.reqres.log;

import lombok.extern.slf4j.Slf4j;
import org.reactivestreams.Publisher;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.stereotype.Component;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilter;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;

@Component
public class RequestResponseLoggingFilter implements WebFilter {

    @Override
    public Mono<Void> filter(ServerWebExchange exchange,  WebFilterChain chain) {

        return DataBufferUtils.join(exchange.getRequest().getBody())
                .flatMap(requestBodyBuffer -> {
                    byte[] requestBytes = new byte[requestBodyBuffer.readableByteCount()];
                    requestBodyBuffer.read(requestBytes);
                    DataBufferUtils.release(requestBodyBuffer);
                    String requestBody = new String(requestBytes, StandardCharsets.UTF_8);

                    logRequest(exchange, requestBody);

                    var cachedRequest = new ServerHttpRequestDecorator(exchange.getRequest()) {
                        @Override
                        public Flux<DataBuffer> getBody() {
                            DataBuffer buffer = exchange.getResponse().bufferFactory().wrap(requestBytes);
                            return Flux.just(buffer);
                        }
                    };

                    var decoratedResponse = new ServerHttpResponseDecorator(exchange.getResponse()) {
                        @Override
                        public Mono<Void> writeWith(Publisher<? extends DataBuffer> body) {
                            if (body instanceof Flux) {
                                Flux<? extends DataBuffer> flux = (Flux<? extends DataBuffer>) body;
                                return flux.collectList().flatMap(dataBuffers -> {
                                    DataBuffer joined = exchange.getResponse().bufferFactory().join(dataBuffers);
                                    byte[] responseBytes = new byte[joined.readableByteCount()];
                                    joined.read(responseBytes);
                                    DataBufferUtils.release(joined);

                                    String responseBody = new String(responseBytes, StandardCharsets.UTF_8);
                                    logResponse(exchange, responseBody);

                                    DataBuffer newBuffer = exchange.getResponse().bufferFactory().wrap(responseBytes);
                                    return super.writeWith(Flux.just(newBuffer));
                                });
                            }
                            return super.writeWith(body);
                        }
                    };

                    return chain.filter(
                            exchange.mutate()
                                    .request(cachedRequest)
                                    .response(decoratedResponse)
                                    .build()
                    );
                });
    }

    private void logRequest(ServerWebExchange exchange, String body) {
        var request = exchange.getRequest();
        System.out.println(">> REQUEST " + request.getMethod() + " " + request.getURI());
        System.out.println(">> Headers: " + request.getHeaders());
        System.out.println(">> Body: " + body);
    }

    private void logResponse(ServerWebExchange exchange, String body) {
        System.out.println("<< RESPONSE for " + exchange.getRequest().getURI());
        System.out.println("<< Body: " + body);
    }
}