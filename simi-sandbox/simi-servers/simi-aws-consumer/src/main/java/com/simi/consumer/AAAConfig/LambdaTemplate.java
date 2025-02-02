package com.simi.consumer.AAAConfig;

import org.springframework.stereotype.Component;
import software.amazon.awssdk.auth.credentials.AwsBasicCredentials;
import software.amazon.awssdk.auth.credentials.StaticCredentialsProvider;
import software.amazon.awssdk.http.async.SdkAsyncHttpClient;
import software.amazon.awssdk.http.nio.netty.NettyNioAsyncHttpClient;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.lambda.LambdaAsyncClient;
import software.amazon.awssdk.services.lambda.model.InvokeRequest;
import software.amazon.awssdk.services.lambda.model.InvokeResponse;
import software.amazon.awssdk.core.SdkBytes;
import java.net.URI;
import java.util.concurrent.CompletableFuture;

@Component
public class LambdaTemplate {
    private LambdaAsyncClient lambdaAsyncClient;
    public  LambdaTemplate() {
        SdkAsyncHttpClient asyncHttpClient = NettyNioAsyncHttpClient.builder().build();
        this.lambdaAsyncClient = LambdaAsyncClient.builder()
                .httpClient(asyncHttpClient)
            .credentialsProvider(StaticCredentialsProvider.create(AwsBasicCredentials.create("test", "test")))
            .endpointOverride(URI.create("http://192.168.127.128:4566"))  // LocalStack Lambda endpoint
            .region(Region.US_EAST_1)
            .build();
    }

    public String invokeLambdaFunction(String functionName, String payload) {
        // Create an InvokeRequest
        InvokeRequest invokeRequest = InvokeRequest.builder()
                .functionName(functionName)
                .payload(SdkBytes.fromUtf8String(payload))
                .build();
        // Invoke the Lambda function
        CompletableFuture<InvokeResponse> invokeResponseCF = lambdaAsyncClient.invoke(invokeRequest);
        InvokeResponse invokeResponse = invokeResponseCF.join();
        return invokeResponse.payload().asUtf8String();  // Return the Lambda function's output
    }
}
