package com.simi.trade.AAAconfig;

import io.swagger.v3.oas.models.ExternalDocumentation;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import org.springdoc.core.models.GroupedOpenApi;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.SpringBootVersion;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Configuration
public class SpringdocOpenapiConfig implements WebMvcConfigurer {
    @Bean
    public GroupedOpenApi oracleApi() {
        return GroupedOpenApi.builder()
                .group("AAA")
                .pathsToMatch("/**")
                .build();
    }

    @Bean
    public GroupedOpenApi actuatorApi() {
        return GroupedOpenApi.builder().group("Actuator")
                .pathsToMatch("/actuator/**")
                .build();
    }

    @Bean
    public OpenAPI springDocOpenAPI(@Value("${server.port:8080}") String port) {
        return new OpenAPI()
                .info(new Info()
                        .title("Api Doc")
                        .description("description")
                        .version("Spring Boot Version: " + SpringBootVersion.getVersion())
                        .license(new License().name("Apache 2.0").url("https://www.apache.org/licenses/LICENSE-2.0.html"))
                )
                .externalDocs(new ExternalDocumentation()
                        .description("SpringDoc Full Documentation")
                        .url("https://springdoc.org")
                );
    }
}
