package com.simi.AAAconfig.bean;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Lookup;

import java.time.LocalTime;

@Slf4j
@Getter
public class SingletonBean {
    @Autowired
    public ObjectProvider<PrototypeBean> prototypeBean;

//    @Lookup
//    public PrototypeBean getPrototypeBean() {
//        return null;
//    }

}
