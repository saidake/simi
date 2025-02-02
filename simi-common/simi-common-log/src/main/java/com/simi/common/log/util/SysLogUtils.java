/*
 * Copyright (c) 2020 pig4cloud Authors. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.simi.common.log.util;

import cn.hutool.core.util.URLUtil;
import cn.hutool.extra.servlet.ServletUtil;
import cn.hutool.http.HttpUtil;
import com.simi.common.log.entity.SysLog;
import jakarta.servlet.http.HttpServletRequest;
import lombok.experimental.UtilityClass;
import org.springframework.core.LocalVariableTableParameterNameDiscoverer;
import org.springframework.expression.EvaluationContext;
import org.springframework.expression.Expression;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.expression.spel.support.StandardEvaluationContext;
import org.springframework.http.HttpHeaders;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.lang.reflect.Method;
import java.util.Objects;

@UtilityClass
public class SysLogUtils {

	public SysLog getSysLog() {
		HttpServletRequest request = ((ServletRequestAttributes) Objects
				.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
		SysLog sysLog = new SysLog();
		sysLog.setType(LogTypeEnum.NORMAL.getType());
//		sysLog.setRemoteAddr(ServletUtil.getClientIP(request));
		sysLog.setRequestUri(URLUtil.getPath(request.getRequestURI()));
		sysLog.setMethod(request.getMethod());
		sysLog.setUserAgent(request.getHeader(HttpHeaders.USER_AGENT));
		sysLog.setParams(HttpUtil.toParams(request.getParameterMap()));
		return sysLog;
	}

	public <T> T getValue(EvaluationContext context, String spelStr, Class<T> clazz) {
		SpelExpressionParser spelExpressionParser = new SpelExpressionParser();
		Expression expression = spelExpressionParser.parseExpression(spelStr);
		return expression.getValue(context, clazz);
	}

	public EvaluationContext getContext(Object[] arguments, Method signatureMethod) {
		String[] parameterNames =  new LocalVariableTableParameterNameDiscoverer().getParameterNames(signatureMethod);
		EvaluationContext context = new StandardEvaluationContext();
		if (parameterNames == null) {
			return context;
		}
		// Populate method parameters into evaluation context
		for (int i = 0; i < arguments.length; i++) {
			context.setVariable(parameterNames[i], arguments[i]);
		}
		return context;
	}

}
