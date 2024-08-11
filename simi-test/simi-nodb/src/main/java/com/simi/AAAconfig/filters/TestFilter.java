//package com.simi.AAAconfig.filters;
//import com.simi.AAAconfig.TestServletRequestWrapper;
//import jakarta.servlet.*;
//import jakarta.servlet.annotation.WebFilter;
//import jakarta.servlet.http.HttpServletRequest;
//import lombok.extern.slf4j.Slf4j;
//
//import java.io.IOException;
//
//@WebFilter(filterName = "testFilter", urlPatterns = "/*")
//@Slf4j
//public class TestFilter implements Filter {
//    @Override
//    public void doFilter(ServletRequest servletRequest, ServletResponse servletResponse, FilterChain filterChain)
//            throws IOException, ServletException {
//        TestServletRequestWrapper testServletRequestWrapper=new TestServletRequestWrapper((HttpServletRequest)servletRequest);
//        filterChain.doFilter(testServletRequestWrapper, servletResponse);
//    }
//
//}
//
