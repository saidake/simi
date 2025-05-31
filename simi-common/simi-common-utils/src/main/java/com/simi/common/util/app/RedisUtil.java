package com.simi.common.util.app;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.concurrent.TimeUnit;

/**
 * Utility class for Redis operations.
 * Provides various methods to interact with Redis for common operations like cache management, hash operations, set and sorted set management, and more.
 */
@Component
public class RedisUtil {

    private static final Logger log = LoggerFactory.getLogger(RedisUtil.class);

    @Autowired
    private RedisTemplate<String, String> redisTemplate;

    /**
     * Sets an expiration time for a specific cache key.
     *
     * @param key  the cache key
     * @param time expiration time in seconds
     * @return true if the operation was successful, false otherwise
     */
    public boolean expire(String key, long time) {
        try {
            if (time > 0) {
                redisTemplate.expire(key, time, TimeUnit.SECONDS);
            }
            return true;
        } catch (Exception e) {
            log.error("Failed to set cache expiration time", e);
            return false;
        }
    }

    /**
     * Retrieves the expiration time of a given cache key.
     *
     * @param key the cache key
     * @return the expiration time in seconds, or 0 if the cache key does not expire
     */
    public long getExpire(String key) {
        return redisTemplate.getExpire(key, TimeUnit.SECONDS);
    }

    /**
     * Checks if a specific cache key exists.
     *
     * @param key the cache key
     * @return true if the key exists, false otherwise
     */
    public boolean hasKey(String key) {
        try {
            return redisTemplate.hasKey(key);
        } catch (Exception e) {
            log.error("Failed to check if key exists", e);
            return false;
        }
    }

    /**
     * Deletes cache entries by their keys.
     *
     * @param key one or more cache keys
     */
    @SuppressWarnings("unchecked")
    public void del(String... key) {
        if (key != null && key.length > 0) {
            if (key.length == 1) {
                redisTemplate.delete(key[0]);
            } else {
                redisTemplate.delete((Collection<String>) CollectionUtils.arrayToList(key));
            }
        }
    }

    /**
     * Deletes a specific cache key.
     *
     * @param key the cache key
     */
    public void delHash(String key) {
        redisTemplate.delete(key);
    }

    /**
     * Deletes cache entries matching a pattern.
     *
     * @param key the pattern to match cache keys
     */
    @SuppressWarnings("unchecked")
    public void batchDel(String key) {
        String keyPattern = key + "*";
        Set<String> keys = redisTemplate.keys(keyPattern);
        redisTemplate.delete(keys);
    }

    //============================ String operations =============================

    /**
     * Retrieves the value for a given cache key.
     *
     * @param key the cache key
     * @return the value associated with the key, or null if the key does not exist
     */
    public Object get(String key) {
        return key == null ? null : redisTemplate.opsForValue().get(key);
    }

    /**
     * Sets a value in the cache for a given key.
     *
     * @param key   the cache key
     * @param value the value to store
     * @return true if the operation was successful, false otherwise
     */
    public boolean set(String key, Object value) {
        try {
            redisTemplate.opsForValue().set(key, String.valueOf(value));
            return true;
        } catch (Exception e) {
            log.error("Failed to set value in cache", e);
            return false;
        }
    }

    /**
     * Sets a value in the cache for a given key with an expiration time.
     *
     * @param key   the cache key
     * @param value the value to store
     * @param time  the expiration time in seconds
     * @return true if the operation was successful, false otherwise
     */
    public boolean set(String key, Object value, long time) {
        try {
            if (time > 0) {
                redisTemplate.opsForValue().set(key, String.valueOf(value), time, TimeUnit.SECONDS);
            } else {
                redisTemplate.opsForValue().set(key, String.valueOf(value));
            }
            return true;
        } catch (Exception e) {
            log.error("Failed to set value with expiration in cache", e);
            return false;
        }
    }

    /**
     * Increments the value of a given cache key by a specified delta.
     *
     * @param key   the cache key
     * @param delta the increment value (must be greater than 0)
     * @return the new value after increment
     */
    public long incr(String key, long delta) {
        if (delta < 0) {
            throw new RuntimeException("Increment delta must be greater than 0");
        }
        return redisTemplate.opsForValue().increment(key, delta);
    }

    /**
     * Decrements the value of a given cache key by a specified delta.
     *
     * @param key   the cache key
     * @param delta the decrement value (must be greater than 0)
     * @return the new value after decrement
     */
    public long decr(String key, long delta) {
        if (delta < 0) {
            throw new RuntimeException("Decrement delta must be greater than 0");
        }
        return redisTemplate.opsForValue().increment(key, -delta);
    }

    //================================ Map operations ============================

    /**
     * Retrieves the value of a specific item from a hash in Redis.
     *
     * @param key  the hash key
     * @param item the item in the hash
     * @return the value associated with the item
     */
    public Object hget(String key, String item) {
        return redisTemplate.opsForHash().get(key, item);
    }

    /**
     * Retrieves all items in a hash from Redis.
     *
     * @param key the hash key
     * @return a map containing all items in the hash
     */
    public Map<Object, Object> hmget(String key) {
        return redisTemplate.opsForHash().entries(key);
    }

    /**
     * Sets multiple items in a hash in Redis.
     *
     * @param key the hash key
     * @param map the map of items to store
     * @return true if the operation was successful, false otherwise
     */
    public boolean hmset(String key, Map<String, Object> map) {
        try {
            redisTemplate.opsForHash().putAll(key, map);
            return true;
        } catch (Exception e) {
            log.error("Failed to set multiple items in hash", e);
            return false;
        }
    }

    /**
     * Sets multiple items in a hash with an expiration time.
     *
     * @param key  the hash key
     * @param map  the map of items to store
     * @param time the expiration time in seconds
     * @return true if the operation was successful, false otherwise
     */
    public boolean hmset(String key, Map<String, Object> map, long time) {
        try {
            redisTemplate.opsForHash().putAll(key, map);
            if (time > 0) {
                expire(key, time);
            }
            return true;
        } catch (Exception e) {
            log.error("Failed to set multiple items in hash with expiration", e);
            return false;
        }
    }

    /**
     * Sets an item in a hash in Redis.
     *
     * @param key   the hash key
     * @param item  the item in the hash
     * @param value the value to store
     * @return true if the operation was successful, false otherwise
     */
    public boolean hset(String key, String item, Object value) {
        try {
            redisTemplate.opsForHash().put(key, item, value);
            return true;
        } catch (Exception e) {
            log.error("Failed to set item in hash", e);
            return false;
        }
    }

    /**
     * Sets an item in a hash with an expiration time.
     *
     * @param key   the hash key
     * @param item  the item in the hash
     * @param value the value to store
     * @param time  the expiration time in seconds
     * @return true if the operation was successful, false otherwise
     */
    public boolean hset(String key, String item, Object value, long time) {
        try {
            redisTemplate.opsForHash().put(key, item, value);
            if (time > 0) {
                expire(key, time);
            }
            return true;
        } catch (Exception e) {
            log.error("Failed to set item in hash with expiration", e);
            return false;
        }
    }

    /**
     * Deletes one or more items from a hash in Redis.
     *
     * @param key  the hash key
     * @param item the item(s) to delete
     */
    public void hdel(String key, Object... item) {
        redisTemplate.opsForHash().delete(key, item);
    }

    /**
     * Checks if an item exists in a hash in Redis.
     *
     * @param key  the hash key
     * @param item the item to check for
     * @return true if the item exists, false otherwise
     */
    public boolean hHasKey(String key, String item) {
        return redisTemplate.opsForHash().hasKey(key, item);
    }

    /**
     * Retrieves all items from a hash in Redis.
     *
     * @param key the hash key
     * @return a set of all items in the hash
     */
    public Set<Object> hKeys(String key) {
        return redisTemplate.opsForHash().keys(key);
    }

    /**
     * Retrieves all values from a hash in Redis.
     *
     * @param key the hash key
     * @return a list of all values in the hash
     */
    public List<Object> hVals(String key) {
        return redisTemplate.opsForHash().values(key);
    }
}
