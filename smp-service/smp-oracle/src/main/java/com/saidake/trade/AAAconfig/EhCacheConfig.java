package com.saidake.trade.AAAconfig;


import lombok.extern.slf4j.Slf4j;
import org.ehcache.Cache;
import org.ehcache.CacheManager;
import org.ehcache.config.CacheConfiguration;
import org.ehcache.config.builders.CacheConfigurationBuilder;
import org.ehcache.config.builders.CacheManagerBuilder;
import org.ehcache.config.builders.ExpiryPolicyBuilder;
import org.ehcache.config.builders.ResourcePoolsBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.time.Duration;

@Configuration
@Slf4j
public class EhCacheConfig {
    @Bean
    public Cache<Long, String> testCache(){
        CacheConfiguration<Long, String> cacheConfiguration= CacheConfigurationBuilder.newCacheConfigurationBuilder(
                Long.class, String.class, ResourcePoolsBuilder.heap(100)
        ).withExpiry(ExpiryPolicyBuilder.timeToIdleExpiration(Duration.ofMinutes(50L))).build();
        CacheManager cacheManager = CacheManagerBuilder.newCacheManagerBuilder().withCache("testCache",cacheConfiguration)
                .build();
        cacheManager.init();
        return cacheManager.getCache("testCache",Long.class, String.class);
    }
}
