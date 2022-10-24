package com.saidake.reflect;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

public class UserInvocationHandler implements InvocationHandler {
    //要增强的实例对象，一个实例对象 >>  InvocationHandler 增强方法对象 >>  最终Proxy代理对象
    UserService userService;

    public UserInvocationHandler(UserService userService){
        this.userService=userService;
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        System.out.println("增强内容");
        return method.invoke(userService,args);
    }
}
