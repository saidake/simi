package com.saidake.geteway.AAAconfig;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang3.StringUtils;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.core.LocalVariableTableParameterNameDiscoverer;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.lang.reflect.Method;
import java.util.List;
import java.util.stream.Collectors;

@Aspect
@Component
@Profile("local")
public class SystemLogAop {
    private static final Logger logger = LoggerFactory.getLogger(SystemLogAop.class);
    @Autowired
    ObjectMapper objectMapper;

    @Around("execution(* com.saidake.*.*.*(..))")
    public void beforeWebMethodExecution(ProceedingJoinPoint joinPoint) throws IOException {
        StringBuilder sb = new StringBuilder(512);
        Long startTime = System.currentTimeMillis();
        // request url
        ServletRequestAttributes attributes = (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
        HttpServletRequest request = attributes.getRequest();
        String methodName = joinPoint.getSignature().getName();
        String className = joinPoint.getTarget().getClass().getName();
        List<String> parameterList = request.getParameterMap().entrySet().stream().map(item -> item.getKey() + " " + StringUtils.join(item.getValue(), ",")).collect(Collectors.toList());

        String resultParameterList = parameterList.size() > 0 ? request.getRequestURI() + "?" + StringUtils.join(parameterList, ",") : "";

        sb.append(System.lineSeparator() + String.format("====== %10s, %6s ======================[REQUEST AOP START]]", resultParameterList, request.getMethod()));
        sb.append(System.lineSeparator() + String.format("METHOD NAME : %s", methodName));
        sb.append(System.lineSeparator() + String.format("CLASS NAME : %s", className));
        if (request.getMethod().equals("GET")) {
            sb.append(System.lineSeparator() + String.format("GET PARAMETERS : %s", objectMapper.writeValueAsString(request.getParameterMap())));
            handleShowResponse(joinPoint, sb, startTime, request);
            return;
        }
        // request parameters
        Object[] args = joinPoint.getArgs();
        MethodSignature signature = (MethodSignature) joinPoint.getSignature();
        Method method = signature.getMethod();
        LocalVariableTableParameterNameDiscoverer u = new LocalVariableTableParameterNameDiscoverer();
        String[] paramNames = u.getParameterNames(method);
        if (args != null && paramNames != null) {
            String params = "";
            for (int i = 0; i < args.length; i++) {
                String argJson = "";
                try {
                    argJson = objectMapper.writeValueAsString(args[i]);
                } catch (Exception e) {
                    e.printStackTrace();
                }
                params += "  " + paramNames[i] + ": " + argJson;
                sb.append(System.lineSeparator() + "BODY            :" + params);
            }
            handleShowResponse(joinPoint, sb, startTime, request);
        }
    }

    private void handleShowResponse(ProceedingJoinPoint joinPoint, StringBuilder sb, Long startTime,
                                    HttpServletRequest request) {
        try {
            Object proceed = joinPoint.proceed();
            sb.append(System.lineSeparator() + "RESPONSE     :" + objectMapper.writeValueAsString(proceed));
            Long costTime = System.currentTimeMillis() - startTime;
            sb.append(System.lineSeparator() + String.format("SPEND TINE : %d ms", costTime));
            sb.append(System.lineSeparator() + String.format("---- $%10s， %6s ---[RECQJEST AOP ED]]", request.getRequestURI(), request.getMethod()));
        } catch (Throwable e) {
            logger.info("method error");
        }
        logger.info(sb.toString());
    }
}