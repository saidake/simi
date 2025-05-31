package com.simi.labs.common.reqres.log;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.buffer.DataBuffer;
import org.springframework.core.io.buffer.DataBufferFactory;
import org.springframework.core.io.buffer.DataBufferUtils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.http.server.reactive.ServerHttpRequestDecorator;
import org.springframework.http.server.reactive.ServerHttpResponse;
import org.springframework.http.server.reactive.ServerHttpResponseDecorator;
import org.springframework.stereotype.Component;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilter;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.stream.Collectors;

/**
 * WebFlux filter for logging request and response information.
 * Logs method, URI, safe headers, and truncated body for requests.
 * Optionally logs status, safe headers, truncated body, and duration for responses
 * based on the 'logging.response.enabled' property.
 * Handles GET requests, ensures response body is preserved, and logs response once.
 * Does not log client IP.
 *
 * @author Craig Brown
 * @since 1.0.1
 */
@Component
public class RequestResponseLoggingFilter implements WebFilter {

    private static final Logger logger = LoggerFactory.getLogger(RequestResponseLoggingFilter.class);
    private static final List<String> SENSITIVE_HEADERS = List.of("Authorization", "Cookie");
    private static final int MAX_REQUEST_BODY_LENGTH = 1000;
    private static final int MAX_RESPONSE_BODY_LENGTH = 1000;

    @Value("${similabs.logging.enable-response-body:false}")
    private boolean logResponseEnabled;

    @Override
    public Mono<Void> filter(ServerWebExchange exchange, WebFilterChain chain) {
        long startTime = System.currentTimeMillis();
        ServerHttpRequest request = exchange.getRequest();
        String requestId = request.getId();

        // Check if request has a body (skip for GET)
        Mono<String> requestBodyMono = request.getMethod().equals(org.springframework.http.HttpMethod.GET)
                ? Mono.just("")
                : DataBufferUtils.join(request.getBody())
                .map(dataBuffer -> {
                    byte[] bytes = new byte[dataBuffer.readableByteCount()];
                    dataBuffer.read(bytes);
                    DataBufferUtils.release(dataBuffer);
                    return new String(bytes, StandardCharsets.UTF_8);
                })
                .defaultIfEmpty("");

        return requestBodyMono.flatMap(requestBody -> {
            // Log request
            logRequest(exchange, requestBody);

            // Cache request body for downstream
            ServerHttpRequestDecorator cachedRequest = new ServerHttpRequestDecorator(request) {
                @Override
                public Flux<DataBuffer> getBody() {
                    if (requestBody.isEmpty()) {
                        return Flux.empty();
                    }
                    DataBuffer buffer = exchange.getResponse().bufferFactory()
                            .wrap(requestBody.getBytes(StandardCharsets.UTF_8));
                    return Flux.just(buffer);
                }
            };

            if (!logResponseEnabled) {
                // Skip response logging and decoration
                return chain.filter(exchange.mutate().request(cachedRequest).build());
            }

            // Store response body for logging
            StringBuilder responseBodyHolder = new StringBuilder();

            // Decorate response
            ServerHttpResponseDecorator decoratedResponse = new ServerHttpResponseDecorator(exchange.getResponse()) {
                @Override
                public Mono<Void> writeWith(org.reactivestreams.Publisher<? extends DataBuffer> body) {
                    DataBufferFactory bufferFactory = getDelegate().bufferFactory();
                    return DataBufferUtils.join(Flux.from(body))
                            .flatMap(dataBuffer -> {
                                byte[] bytes = new byte[dataBuffer.readableByteCount()];
                                dataBuffer.read(bytes);
                                DataBufferUtils.release(dataBuffer);
                                responseBodyHolder.append(new String(bytes, StandardCharsets.UTF_8));
                                DataBuffer newBuffer = bufferFactory.wrap(bytes);
                                return super.writeWith(Mono.just(newBuffer));
                            })
                            .switchIfEmpty(Mono.defer(() -> {
                                responseBodyHolder.append("");
                                return super.writeWith(Mono.empty());
                            }));
                }
            };

            // Log response before commit
            ServerHttpResponse response = decoratedResponse.getDelegate();
            response.beforeCommit(() -> {
                String responseBody = responseBodyHolder.toString();
                logResponse(exchange, responseBody, startTime);
                return Mono.empty();
            });

            return chain.filter(exchange.mutate()
                    .request(cachedRequest)
                    .response(decoratedResponse)
                    .build());
        });
    }

    private void logRequest(ServerWebExchange exchange, String body) {
        ServerHttpRequest request = exchange.getRequest();
        String headers = getSafeHeaders(request.getHeaders());
        logger.info("""
                
                >> Incoming Request [{}]
                -> Method: {}
                -> URI:    {}
                -> Headers: {}
                -> Body:   {}
                """,
                request.getId(),
                request.getMethod(),
                request.getURI(),
                headers,
                body.isEmpty() ? "<empty>" : truncateBody(body, MAX_REQUEST_BODY_LENGTH));
    }

    private void logResponse(ServerWebExchange exchange, String body, long startTime) {
        ServerHttpResponse response = exchange.getResponse();
        long duration = System.currentTimeMillis() - startTime;
        String headers = getSafeHeaders(response.getHeaders());
        logger.info("""
                
                << Outgoing Response [{}]
                -> URI:     {}
                -> Status:  {}
                -> Headers: {}
                -> Body:    {}
                -> Duration: {}ms
                """,
                exchange.getRequest().getId(),
                exchange.getRequest().getURI(),
                response.getStatusCode(),
                headers,
                body.isEmpty() ? "<empty>" : truncateBody(body, MAX_RESPONSE_BODY_LENGTH),
                duration);
    }

    private String getSafeHeaders(HttpHeaders headers) {
        return headers.entrySet().stream()
                .filter(entry -> !SENSITIVE_HEADERS.contains(entry.getKey()))
                .map(entry -> entry.getKey() + ": " + entry.getValue())
                .collect(Collectors.joining(", "));
    }

    private String truncateBody(String body, int maxLength) {
        if (body.length() > maxLength) {
            return body.substring(0, maxLength) + "...";
        }
        return body;
    }
}