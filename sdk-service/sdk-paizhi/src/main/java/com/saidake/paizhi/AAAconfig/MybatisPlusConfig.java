package com.saidake.paizhi.AAAconfig;


import com.baomidou.mybatisplus.annotation.DbType;
import com.baomidou.mybatisplus.extension.plugins.MybatisPlusInterceptor;
import com.baomidou.mybatisplus.extension.plugins.inner.PaginationInnerInterceptor;
import org.mybatis.spring.annotation.MapperScan;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

//Spring boot方式
@Configuration
//此处需要配置扫描，否则运行可能运行失败
@MapperScan("com.saidake.mybatis.mapper")
public class MybatisPlusConfig {
    //可在controller同级目录下新建一个config包，
    //再创建一个MybatisPlusConfig类，
    //将最新版@bean和以下的内容直接复制到类中
    // 最新版
    @Bean
    public MybatisPlusInterceptor mybatisPlusInterceptor() {
        MybatisPlusInterceptor interceptor = new MybatisPlusInterceptor();
        interceptor.addInnerInterceptor(new PaginationInnerInterceptor(DbType.H2));
        return interceptor;
    }

}
