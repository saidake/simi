package com.saidake;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

public class ProxyJdkTest {

    public static void main(String[] args) {
        System.getProperties().put("jdk.proxy.ProxyGenerator.saveGeneratedFiles", "true");
        Movie target = new Movie();
        IMovie proxyInstance = (IMovie) new MovieProxy(target).getProxyInstance();
        System.out.println("proxyInstance="+proxyInstance.getClass());
        proxyInstance.advertising(true,"开始消息");
        proxyInstance.play(" 速度与激情8 ");
        proxyInstance.advertising(false,"结束消息");
    }

    public  interface IMovie {
        void play(String movieName);
        void advertising(Boolean isBoforMovie,String txt);
    }
    public static class Movie implements IMovie {
        @Override
        public void play(String movieName) {
            System.out.println("播放电影《"+movieName+"》");
        }

        @Override
        public void advertising(Boolean isBoforMovie, String txt) {
            if(isBoforMovie){
                System.out.println("影片马上开始,"+txt);
            }else{
                System.out.println("影片正片已经结束"+txt);
            }
        }
    }
    public static class MovieProxy {
        private Object target;

        public MovieProxy(Object target) {
            this.target = target;
        }
        public Object getProxyInstance(){
            return Proxy.newProxyInstance(target.getClass().getClassLoader(),
                    target.getClass().getInterfaces(),
                    new InvocationHandler() {
                        @Override
                        public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
                            System.out.println("JDK代理开始~~");
                            //反射机制调用目标对象的方法
                            Object ret = method.invoke(target,args);
                            System.out.println("JDK代理结束~~");
                            return ret;
                        }
                    });
        }
    }

}
