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

@Aspect
@Slf4j
public class SysLogAspect {

    @Around("@annotation(sysLog)")
    @SneakyThrows
    public Object around(ProceedingJoinPoint point, com.simi.common.log.annotation.SysLog sysLog) {
        String strClassName = point.getTarget().getClass().getName();
        String strMethodName = point.getSignature().getName();

        String value = sysLog.value();
        String expression = sysLog.expression();
        // 当前表达式存在 SPEL，会覆盖 value 的值
        if (StrUtil.isNotBlank(expression)) {
            // 解析SPEL
            MethodSignature signature = (MethodSignature) point.getSignature();
            EvaluationContext context = SysLogUtils.getContext(point.getArgs(), signature.getMethod());
            try {
                value = SysLogUtils.getValue(context, expression, String.class);
            }
            catch (Exception e) {
                // SPEL 表达式异常，获取 value 的值
                log.info("@SysLog  {} Exception.", expression);
            }
        }

        SysLog logVo = SysLogUtils.getSysLog();
        logVo.setTitle(value);

        // 发送异步日志事件
        Long startTime = System.currentTimeMillis();
        Object obj;

        try {
            obj = point.proceed();
        }
        catch (Exception e) {
            logVo.setType(LogTypeEnum.ERROR.getType());
            logVo.setException(e.getMessage());
            throw e;
        }
        finally {
            Long endTime = System.currentTimeMillis();
            logVo.setTime(endTime - startTime);
            SpringContextUtil.publishEvent(new SysLogEvent(logVo));
        }

        return obj;
    }

}
