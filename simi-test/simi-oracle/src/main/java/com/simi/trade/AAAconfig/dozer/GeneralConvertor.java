package com.simi.trade.AAAconfig.dozer;

import jakarta.annotation.Resource;
import lombok.NoArgsConstructor;
import org.dozer.CustomConverter;
import org.dozer.DozerConverter;
import org.dozer.Mapper;
import org.springframework.stereotype.Component;

//public class GeneralConvertor extends DozerConverter<Person, DingDong> {
//    public GeneralConvertor(Class<Person> prototypeA, Class<DingDong> prototypeB) {
//        super(prototypeA, prototypeB);
//    }
//    @Override
//    public DingDong convertTo(Person person, DingDong dingDong) {
//        System.out.println(person);
//        return null;
//    }
//
//    @Override
//    public Person convertFrom(DingDong dingDong, Person person) {
//        System.out.println(dingDong);
//        return null;
//    }
//}

public class GeneralConvertor implements CustomConverter {

    @Override
    public Object convert(Object o, Object o1, Class<?> aClass, Class<?> aClass1) {
        System.out.println(o+"ggggggggggggggggggg");
        return null;
    }
}
