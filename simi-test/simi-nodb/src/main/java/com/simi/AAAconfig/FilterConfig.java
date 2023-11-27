//package com.simi.AAAconfig;
//
//import com.simi.AAAconfig.filters.TestFilter;
//import org.springframework.boot.web.servlet.FilterRegistrationBean;
//import org.springframework.context.annotation.Bean;
//import org.springframework.context.annotation.Configuration;
//
//import java.util.ArrayList;
//import java.util.HashMap;
//import java.util.List;
//import java.util.Map;
//
//@Configuration
//public class FilterConfig {
//
//    @Bean
//    public FilterRegistrationBean<TestFilter> requestFilterRegistration() {
//        FilterRegistrationBean<TestFilter> registration = new FilterRegistrationBean<>();
//        // 将过滤器配置到FilterRegistrationBean对象中
//        registration.setFilter(new TestFilter());
//        // 给过滤器取名
//        registration.setName("requestFilter");
//        // 设置过滤器优先级，该值越小越优先被执行
//        registration.setOrder(1);
//        Map<String, String> paramMap = new HashMap<>();
//        paramMap.put("noFilterUrl", "/test");
//        // 设置initParams参数
//        registration.setInitParameters(paramMap);
//        List<String> urlPatterns = new ArrayList<>();
//        urlPatterns.add("/*");
//        // 设置urlPatterns参数
//        registration.setUrlPatterns(urlPatterns);
//        return registration;
//    }
//}
