package com.simi;

import org.springframework.cglib.proxy.Enhancer;
import org.springframework.cglib.proxy.MethodInterceptor;
import org.springframework.cglib.proxy.MethodProxy;

import java.lang.reflect.Method;

public class ProxyCglibTest {
    public static void main(String[] args) {
        Movie target = new Movie();     //创建目标对象
        Movie proxyInstance = (Movie)new MovieProxy(target).getProxyInstance(); //获取到代理对象，并且将目标对象传递给代理对象
        proxyInstance.advertising(true,"开始消息");
        proxyInstance.play(" 速度与激情8 ");  //执行代理对象的方法，触发intecept 方法，从而实现 对目标对象的调用
        proxyInstance.advertising(false,"结束消息");
    }


    public static class Movie {
        public void play(String movieName) {
            System.out.println("我是cglib代理,不需要实现接口,您正在观看电影《"+movieName+"》");
        }

        public void advertising(Boolean isBoforMovie, String txt) {
            if(isBoforMovie){
                System.out.println("影片马上开始,"+txt);
            }else{
                System.out.println("影片正片已经结束"+txt);
            }
        }
    }

    public static class MovieProxy implements MethodInterceptor {
        //维护一个目标对象
        private Object target;

        //构造器，传入一个被代理的对象
        public MovieProxy(Object target) {
            this.target = target;
        }

        //返回一个代理对象:  是 target 对象的代理对象
        public Object getProxyInstance() {
            Enhancer enhancer = new Enhancer();           //  创建一个工具类
            enhancer.setSuperclass(target.getClass());    // 设置代理目标类
            enhancer.setCallback(this);   // 设置回调函数
            return enhancer.create();     // 创建子类对象，即代理对象
        }


        @Override
        public Object intercept(Object o, Method method, Object[] objects, MethodProxy methodProxy) throws Throwable {
            System.out.println("Cglib代理开始~~");
            Object returnVal = method.invoke(target,objects);
            System.out.println("Cglib代理结束~~");
            return returnVal;
        }
    }

}
