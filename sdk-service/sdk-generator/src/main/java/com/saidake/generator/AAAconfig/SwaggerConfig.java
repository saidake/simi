package com.saidake.generator.AAAconfig;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.core.Ordered;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.ViewControllerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;
import springfox.documentation.builders.ApiInfoBuilder;
import springfox.documentation.builders.PathSelectors;
import springfox.documentation.builders.RequestHandlerSelectors;
import springfox.documentation.oas.annotations.EnableOpenApi;
import springfox.documentation.service.ApiInfo;
import springfox.documentation.service.Contact;
import springfox.documentation.spi.DocumentationType;
import springfox.documentation.spring.web.plugins.Docket;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;

@Configuration
@EnableOpenApi
@Profile({"prod","test","bluetooth-app"})
public class SwaggerConfig  implements WebMvcConfigurer {
    @Bean
    public Docket docket(){
        return new Docket(DocumentationType.SWAGGER_2)
                .apiInfo(apiInfo())
                .enable(true)
                .select()
                .apis(RequestHandlerSelectors.basePackage("com.saidake.generator.com.saidake.citi.controller"))
                .paths(PathSelectors.any())
                .build();
    }

    private ApiInfo apiInfo(){
        return new ApiInfoBuilder()
                .title("XX项目接口文档")
                .description("XX项目描述")
                .contact(new Contact("作者", "作者URL", "作者Email"))
                .version("1.0")
                .build();
    }

    @Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {
        registry.addResourceHandler("/static/**").addResourceLocations("classpath:/META-INF/resources/");
    }
    private static List<String> staticSuffixList= Arrays.asList(".js",".css",".html");


    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        registry.addInterceptor(new HandlerInterceptor() {
            @Override
            public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) throws Exception {
                String uri = request.getRequestURI();
                System.out.println("start check: "+uri);
                if(Pattern.matches("/test/.*?",uri)&& staticSuffixList.stream().anyMatch(uri::endsWith)){
                    System.out.println("forward: "+request.getRequestURI().replace("/test/","/static/"));
                    request.getRequestDispatcher(request.getRequestURI().replace("/test/","/static/")).forward(request,response);
                    return false;
                }
                return true;
            }
        });

    }

    @Override
    public void addViewControllers(ViewControllerRegistry registry){
        registry.addViewController("/test/swagger-resources/configuration/ui").setViewName("forward:/swagger-resources/configuration/ui");             //设置访问路径为 “/” 跳转到指定页面
        registry.addViewController("/test/swagger-resources").setViewName("forward:/swagger-resources");      //设置访问路径为 “/” 跳转到指定页面
        registry.addViewController("/test/v2/api-docs").setViewName("forward:/v2/api-docs");                          //设置访问路径为 “/” 跳转到指定页面
        registry.addViewController("/test/swagger").setViewName("forward:/static/doc.html");
        registry.setOrder(Ordered.HIGHEST_PRECEDENCE);
    }
}
