package com.saidake.common.core.config.http;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;
import org.springframework.core.task.TaskDecorator;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;

import java.util.Map;

@Slf4j
    public class ServletAttributesTaskDecorator implements TaskDecorator {

    @Override
    public Runnable decorate(Runnable runnable) {
        ServletAttributesDecoratorEntity t = preHandleRunnable();
        return handleRunnable(runnable, t);
    }

    public Runnable handleRunnable(Runnable runnable, ServletAttributesDecoratorEntity servletAttributesDecoratorEntity) {
        return () -> {
            try {
                long start = System.currentTimeMillis();
                RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
                if (requestAttributes == null) {
                    RequestContextHolder.setRequestAttributes(servletAttributesDecoratorEntity.getRequestAttributes());
                }
                Runnable mdcTask =handleMDCRunnable(runnable, servletAttributesDecoratorEntity.getContextMap());
                mdcTask.run();
//                log.info("Thread {} execute complete, cast {} ms.", Thread.currentThread().getName(), System.currentTimeMillis() - start);
            } catch (Exception e) {
                log.error("Thread {} occurred error, message is {}", Thread.currentThread().getName(), e.getMessage());
            } finally {
                RequestContextHolder.resetRequestAttributes();
            }
        };
    }

    public ServletAttributesDecoratorEntity preHandleRunnable() {
        RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
        Map<String, String> contextMap = preMDCHandleRunnable();
        return new ServletAttributesDecoratorEntity(contextMap, requestAttributes);
    }

    /**
     * Used by {@link ServletAttributesTaskDecorator}, a simple pojo.
     */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    class ServletAttributesDecoratorEntity {
        // Context map from MD5.
        private Map<String, String> contextMap;

        // Get from spring mvc DispatcherServlet.
        private RequestAttributes requestAttributes;
    }



    public Runnable handleMDCRunnable(Runnable runnable, Map<String, String> contextMap) {
        return () -> {
            try {
                // @Async thread context
                // (Restore context's MDC data)
                if (contextMap != null) {
                    MDC.setContextMap(contextMap);
                }
                runnable.run();
            } finally {
                MDC.clear();
            }
        };
    }

    public Map<String, String> preMDCHandleRunnable() {
        return MDC.getCopyOfContextMap();
    }
}