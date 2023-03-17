package com.saidake.trade.AAAconfig;

import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.models.ExternalDocumentation;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.parameters.Parameter;
import org.springdoc.core.customizers.OpenApiCustomiser;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.SpringBootVersion;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import jakarta.annotation.PostConstruct;

@Configuration
public class SpringdocOpenapiConfig implements WebMvcConfigurer {

    @PostConstruct
    private void configProperties(){
        System.setProperty("springdoc.swagger-ui.docExpansion","none");
        System.setProperty("springdoc.swagger-ui.path","/swagger");
        System.setProperty("springdoc.api-docs.enabled","true");
        System.setProperty("springdoc.api-docs.path","/swagger-ui/api-docs");
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

    /**
     * 添加全局的请求头参数
     */
    @Bean
    public OpenApiCustomiser customerGlobalHeaderOpenApiCustomiser() {
        return openApi -> openApi.getPaths().values().stream().flatMap(pathItem -> pathItem.readOperations().stream())
                .forEach(operation -> {
                    Parameter customHeaderVersion = new Parameter().in(ParameterIn.HEADER.toString()).name("Custom-Header-Version")
                            .description("jwt token").schema(new StringSchema()).example("v1").required(false);
                    operation.addParametersItem(customHeaderVersion);
                });
    }

}
