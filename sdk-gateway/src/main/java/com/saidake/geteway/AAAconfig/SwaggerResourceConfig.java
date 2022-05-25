package com.saidake.geteway.AAAconfig;

import lombok.AllArgsConstructor;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.cloud.gateway.config.GatewayProperties;
import org.springframework.cloud.gateway.route.RouteLocator;
import org.springframework.cloud.gateway.support.NameUtils;
import org.springframework.context.annotation.Primary;
import org.springframework.stereotype.Component;
import springfox.documentation.swagger.web.SwaggerResource;
import springfox.documentation.swagger.web.SwaggerResourcesProvider;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * 跨域配置
 *
 */
@Component
@Primary
@EnableConfigurationProperties(SwaggerAggProperties.class)
public class SwaggerResourceConfig implements SwaggerResourcesProvider {   // 此类定义了swagger网关层的开放接口，在访问swagger-ui中会拉去此接口的数据。
    private final RouteLocator routeLocator;
    private final GatewayProperties gatewayProperties;

    @Resource
    private SwaggerAggProperties swaggerAggProperties;

    public SwaggerResourceConfig(RouteLocator routeLocator, GatewayProperties gatewayProperties) {
        this.routeLocator = routeLocator;
        this.gatewayProperties = gatewayProperties;
    }

    @Override
    public List<SwaggerResource> get() {
        List<SwaggerResource> resources = new ArrayList<>();
        Set<String> routes = new HashSet<>();

        routeLocator.getRoutes().subscribe(route -> routes.add(route.getId()));        //取出Spring Cloud Gateway中的route
        gatewayProperties.getRoutes().stream().filter(                                 //结合application.yml中的路由配置，只获取有效的route节点
                routeDefinition -> (
                        routes.contains(routeDefinition.getId()) && swaggerAggProperties.isShow(routeDefinition.getId())
                )
        ).forEach(routeDefinition -> routeDefinition.getPredicates().stream()
                .filter(predicateDefinition -> ("Path").equalsIgnoreCase(predicateDefinition.getName()))
                .forEach(predicateDefinition -> resources.add(
                        swaggerResource(
                                routeDefinition.getId(),
                                predicateDefinition.getArgs().get(NameUtils.GENERATED_NAME_PREFIX + "0").replace("/**", swaggerAggProperties.getApiDocsPath())
                        )
                        )
                )
        );
        System.out.println("resources:"+resources);
        return resources;
    }

    private SwaggerResource swaggerResource(String name, String location) {
        SwaggerResource swaggerResource = new SwaggerResource();
        swaggerResource.setName(name);
        swaggerResource.setLocation(location);
        swaggerResource.setSwaggerVersion(swaggerAggProperties.getSwaggerVersion());
        return swaggerResource;
    }
}