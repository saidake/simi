package com.simi.common.log.aspect;

import cn.hutool.core.util.StrUtil;
import com.simi.common.util.app.SpringContextUtil;
import com.simi.common.log.entity.SysLog;
import com.simi.common.log.event.SysLogEvent;
import com.simi.common.log.util.LogTypeEnum;
import com.simi.common.log.util.SysLogUtils;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.expression.EvaluationContext;

/**
 * Aspect class to handle logging functionality around methods annotated with @SysLog.
 * This class logs method execution details, including the method name, execution time,
 * and exception details if any, and publishes the log to an event bus for further handling.
 */
@Aspect
@Slf4j
public class SysLogAspect {

    /**
     * Around advice to intercept methods annotated with @SysLog and log execution details.
     * This method handles the logging logic, such as the method execution time and exceptions.
     * It also supports the evaluation of expressions within the annotation using Spring Expression Language (SPEL).
     *
     * @param point the ProceedingJoinPoint that allows us to proceed with the method execution
     * @param sysLog the SysLog annotation applied to the method
     * @return the result of the method execution
     * @throws Throwable if the method execution throws an exception
     */
    @Around("@annotation(sysLog)")  // Intercepts methods annotated with @SysLog
    @SneakyThrows
    public Object around(ProceedingJoinPoint point, com.simi.common.log.annotation.SysLog sysLog) {
        // Retrieve the class and method name from the ProceedingJoinPoint
        String strClassName = point.getTarget().getClass().getName();
        String strMethodName = point.getSignature().getName();

        // Get the value from the SysLog annotation or use the expression if provided
        String value = sysLog.value();
        String expression = sysLog.expression();

        // If an expression is provided in the annotation, evaluate it using Spring's SpEL
        if (StrUtil.isNotBlank(expression)) {
            // Create a MethodSignature to retrieve method information
            MethodSignature signature = (MethodSignature) point.getSignature();
            EvaluationContext context = SysLogUtils.getContext(point.getArgs(), signature.getMethod());

            try {
                // Evaluate the expression and set the value
                value = SysLogUtils.getValue(context, expression, String.class);
            } catch (Exception e) {
                log.info("@SysLog  {} Exception.", expression);  // Log any exception encountered during SpEL evaluation
            }
        }

        SysLog logVo = SysLogUtils.getSysLog();
        logVo.setTitle(value);

        Long startTime = System.currentTimeMillis();
        Object obj;

        try {
            obj = point.proceed();
        }
        catch (Exception e) {
            logVo.setType(LogTypeEnum.ERROR.getType());
            logVo.setException(e.getMessage());
            // Rethrow the exception to allow it to propagate
            throw e;
        }
        finally {
            Long endTime = System.currentTimeMillis();
            logVo.setTime(endTime - startTime);

            // Publish the SysLog event to be handled asynchronously
            SpringContextUtil.publishEvent(new SysLogEvent(logVo));
        }

        return obj;
    }
}
