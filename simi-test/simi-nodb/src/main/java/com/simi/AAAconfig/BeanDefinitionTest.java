package com.simi.AAAconfig;

import com.simi.AAAconfig.bean.PrototypeBean;
import com.simi.AAAconfig.bean.SingletonBean;
import com.simi.common.test.pojo.Person;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;

@Configuration
public class BeanDefinitionTest {



    @Bean
    public SingletonBean singletonBean() {
        return new SingletonBean();
    }

}
