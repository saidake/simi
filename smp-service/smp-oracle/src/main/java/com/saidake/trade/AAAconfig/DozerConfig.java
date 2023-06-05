package com.saidake.trade.AAAconfig;

import com.google.common.collect.Maps;
import com.saidake.trade.AAAconfig.dozer.GeneralConvertor;
import org.dozer.CustomConverter;
import org.dozer.DozerBeanMapper;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

@Configuration
public class DozerConfig {
    @Bean
    public DozerBeanMapper dozerBeanMapper() {
        List<String> mappingFiles = Arrays.asList("dozer/dozer-mapping.xml");
        DozerBeanMapper dozerBean = new DozerBeanMapper();
        final Map<String, CustomConverter> customConverterMap = Maps.newHashMap();
        customConverterMap.put("typeConverter", new GeneralConvertor());
        dozerBean.setCustomConvertersWithId(customConverterMap);
        //dozerBean.setCustomConverters(Arrays.asList(new GeneralConvertor()));
        dozerBean.setMappingFiles(mappingFiles);
        return dozerBean;
    }
}
