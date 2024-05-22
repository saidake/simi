//package com.simi.common.util.server;
//
//import org.slf4j.Logger;
//import org.slf4j.LoggerFactory;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.dao.DataAccessException;
//import org.springframework.data.redis.connection.RedisConnection;
//import org.springframework.data.redis.core.RedisCallback;
//import org.springframework.data.redis.core.RedisTemplate;
//import org.springframework.data.redis.core.ZSetOperations;
//import org.springframework.data.redis.serializer.RedisSerializer;
//import org.springframework.stereotype.Component;
//import org.springframework.util.CollectionUtils;
//import redis.clients.jedis.Jedis;
//import redis.clients.jedis.JedisCluster;
//import redis.clients.jedis.Protocol;
//import redis.clients.jedis.commands.JedisCommands;
//import redis.clients.jedis.util.SafeEncoder;
//
//import java.io.Serializable;
//import java.util.*;
//import java.util.concurrent.TimeUnit;
//
///**
// * Redis工具类
// */
//@Component
//public class RedisUtil {
//
//    private static final Logger log = LoggerFactory.getLogger(RedisUtil.class);
//
//    @Autowired
//    private RedisTemplate<String, String> redisTemplate;
//
//    /**
//     * 指定缓存失效时间
//     *
//     * @param key  键
//     * @param time 时间(秒)
//     * @return
//     */
//    public boolean expire(String key, long time) {
//        try {
//            if (time > 0) {
//                redisTemplate.expire(key, time, TimeUnit.SECONDS);
//            }
//            return true;
//        } catch (Exception e) {
//            log.error("指定缓存失效时间失败", e);
//            return false;
//        }
//    }
//
//    /**
//     * 根据key 获取过期时间
//     *
//     * @param key 键 不能为null
//     * @return 时间(秒) 返回0代表为永久有效
//     */
//    public long getExpire(String key) {
//        return redisTemplate.getExpire(key, TimeUnit.SECONDS);
//    }
//
//    /**
//     * 判断key是否存在
//     *
//     * @param key 键
//     * @return true 存在 false不存在
//     */
//    public boolean hasKey(String key) {
//        try {
//            return redisTemplate.hasKey(key);
//        } catch (Exception e) {
//            log.error("判断key是否存在失败", e);
//            return false;
//        }
//    }
//
//    /**
//     * 删除缓存
//     *
//     * @param key 可以传一个值 或多个
//     */
//    @SuppressWarnings("unchecked")
//    public void del(String... key) {
//        if (key != null && key.length > 0) {
//            if (key.length == 1) {
//                redisTemplate.delete(key[0]);
//            } else {
//                redisTemplate.delete((Collection<String>) CollectionUtils.arrayToList(key));
//            }
//        }
//    }
//
//    public void delHash(String key) {
//        redisTemplate.delete(key);
//    }
//
//    /**
//     * 删除缓存
//     *
//     * @param key 可以传一个值 或多个
//     */
//    @SuppressWarnings("unchecked")
//    public void batchDel(String key) {
//        String key2 = key + "*";
//        Set<String> keys = redisTemplate.keys(key2);
//        redisTemplate.delete(keys);
//    }
//
//    //============================String=============================
//
//    /**
//     * 普通缓存获取
//     *
//     * @param key 键
//     * @return 值
//     */
//    public Object get(String key) {
//        return key == null ? null : redisTemplate.opsForValue().get(key);
//    }
//
//    /**
//     * 普通缓存放入
//     *
//     * @param key   键
//     * @param value 值
//     * @return true成功 false失败
//     */
//    public boolean set(String key, Object value) {
//        try {
//            redisTemplate.opsForValue().set(key, String.valueOf(value));
//            return true;
//        } catch (Exception e) {
//            log.error("普通缓存放入失败", e);
//            return false;
//        }
//
//    }
//
//    /**
//     * 普通缓存放入并设置时间
//     *
//     * @param key   键
//     * @param value 值
//     * @param time  时间(秒) time要大于0 如果time小于等于0 将设置无限期
//     * @return true成功 false 失败
//     */
//    public boolean set(String key, Object value, long time) {
//        try {
//            if (time > 0) {
//                redisTemplate.opsForValue().set(key, String.valueOf(value), time, TimeUnit.SECONDS);
//            } else {
//                redisTemplate.opsForValue().set(key, String.valueOf(value));
//            }
//            return true;
//        } catch (Exception e) {
//            log.error("普通缓存放入并设置时间失败", e);
//            return false;
//        }
//    }
//
//    /**
//     * 递增
//     *
//     * @param key 键
//     * @return
//     */
//    public long incr(String key, long delta) {
//        if (delta < 0) {
//            throw new RuntimeException("递增因子必须大于0");
//        }
//        return redisTemplate.opsForValue().increment(key, delta);
//    }
//
//    /**
//     * 递减
//     *
//     * @param key 键
//     * @return
//     */
//    public long decr(String key, long delta) {
//        if (delta < 0) {
//            throw new RuntimeException("递减因子必须大于0");
//        }
//        return redisTemplate.opsForValue().increment(key, -delta);
//    }
//
//    //================================Map=================================
//
//    /**
//     * HashGet
//     *
//     * @param key  键 不能为null
//     * @param item 项 不能为null
//     * @return 值
//     */
//    public Object hget(String key, String item) {
//        return redisTemplate.opsForHash().get(key, item);
//    }
//
//    /**
//     * 获取hashKey对应的所有键值
//     *
//     * @param key 键
//     * @return 对应的多个键值
//     */
//    public Map<Object, Object> hmget(String key) {
//        return redisTemplate.opsForHash().entries(key);
//    }
//
//    /**
//     * HashSet
//     *
//     * @param key 键
//     * @param map 对应多个键值
//     * @return true 成功 false 失败
//     */
//    public boolean hmset(String key, Map<String, Object> map) {
//        try {
//            redisTemplate.opsForHash().putAll(key, map);
//            return true;
//        } catch (Exception e) {
//            log.error("hmset失败", e);
//            return false;
//        }
//    }
//
//    /**
//     * HashSet 并设置时间
//     *
//     * @param key  键
//     * @param map  对应多个键值
//     * @param time 时间(秒)
//     * @return true成功 false失败
//     */
//    public boolean hmset(String key, Map<String, Object> map, long time) {
//        try {
//            redisTemplate.opsForHash().putAll(key, map);
//            if (time > 0) {
//                expire(key, time);
//            }
//            return true;
//        } catch (Exception e) {
//            log.error("hmset失败", e);
//            return false;
//        }
//    }
//
//    /**
//     * 向一张hash表中放入数据,如果不存在将创建
//     *
//     * @param key   键
//     * @param item  项
//     * @param value 值
//     * @return true 成功 false失败
//     */
//    public boolean hset(String key, String item, Object value) {
//        try {
//            redisTemplate.opsForHash().put(key, item, value);
//            return true;
//        } catch (Exception e) {
//            log.error("hset失败", e);
//            return false;
//        }
//    }
//
//    /**
//     * 向一张hash表中放入数据,如果不存在将创建
//     *
//     * @param key   键
//     * @param item  项
//     * @param value 值
//     * @param time  时间(秒)  注意:如果已存在的hash表有时间,这里将会替换原有的时间
//     * @return true 成功 false失败
//     */
//    public boolean hset(String key, String item, Object value, long time) {
//        try {
//            redisTemplate.opsForHash().put(key, item, value);
//            if (time > 0) {
//                expire(key, time);
//            }
//            return true;
//        } catch (Exception e) {
//            log.error("hset失败", e);
//            return false;
//        }
//    }
//
//    /**
//     * 删除hash表中的值
//     *
//     * @param key  键 不能为null
//     * @param item 项 可以使多个 不能为null
//     */
//    public void hdel(String key, Object... item) {
//        redisTemplate.opsForHash().delete(key, item);
//    }
//
//    /**
//     * 判断hash表中是否有该项的值
//     *
//     * @param key  键 不能为null
//     * @param item 项 不能为null
//     * @return true 存在 false不存在
//     */
//    public boolean hHasKey(String key, String item) {
//        return redisTemplate.opsForHash().hasKey(key, item);
//    }
//
//    /**
//     * hash递增 如果不存在,就会创建一个 并把新增后的值返回
//     *
//     * @param key  键
//     * @param item 项
//     * @param by   要增加几(大于0)
//     * @return
//     */
//    public double hincr(String key, String item, double by) {
//        log.info("redis-->hash递增key:" + key + "||item:" + item + "||by:" + by);
//        return redisTemplate.opsForHash().increment(key, item, by);
//    }
//
//    /**
//     * hash递减
//     *
//     * @param key  键
//     * @param item 项
//     * @param by   要减少记(小于0)
//     * @return
//     */
//    public double hdecr(String key, String item, double by) {
//        log.info("redis-->hash递减key:" + key + "||item:" + item + "||by:" + by);
//        return redisTemplate.opsForHash().increment(key, item, -by);
//    }
//
//    /**
//     * 移除值为value的
//     *
//     * @param key    键
//     * @param values 值 可以是多个
//     * @return 移除的个数
//     */
//    public long setRemove(String key, Object... values) {
//        try {
//            Long count = redisTemplate.opsForSet().remove(key, values);
//            return count;
//        } catch (Exception e) {
//            log.error("异常信息为：",e);
//            return 0;
//        }
//    }
//
//    public void zSetAdd(String key, String value) {
//        // 获取已缓存的最近浏览的职位
//        ZSetOperations<String, String> zSetOperations = redisTemplate.opsForZSet();
//        // zset内部是按分数来排序的，这里用当前时间做分数
//        zSetOperations.add(key, value, System.currentTimeMillis());
//
//    }
//
//    public void zSetRemove(String key, String value) {
//        // 获取已缓存的最近浏览的职位
//        ZSetOperations<String, String> zSetOperations = redisTemplate.opsForZSet();
//        // zset内部是按分数来排序的，这里用当前时间做分数
//        zSetOperations.remove(key, value);
//
//    }
//
//
//    public boolean setIfAbsent(final String key, Serializable value, long exptime) {
//        Boolean b = (Boolean) redisTemplate.execute(new RedisCallback<Boolean>() {
//            @Override
//            public Boolean doInRedis(RedisConnection connection) throws DataAccessException {
//                RedisSerializer valueSerializer = redisTemplate.getValueSerializer();
//                RedisSerializer keySerializer = redisTemplate.getKeySerializer();
//                Object obj = connection.execute("set", keySerializer.serialize(key),
//                        valueSerializer.serialize(value),
//                        SafeEncoder.encode("NX"),
//                        SafeEncoder.encode("EX"),
//                        Protocol.toByteArray(exptime));
//                return obj != null;
//            }
//        });
//        return b;
//    }
//
//    public static final String UNLOCK_LUA = "if redis.call('get',KEYS[1]) == ARGV[1] then return redis.call('del',KEYS[1]) else return 0 end";
//
//    public static final String STOCK_LUA = "local value = redis.call('hget', KEYS[1], ARGV[1]) if not value then return -1  end value = tonumber(value)  local decrby = tonumber(ARGV[2])  if value >= decrby then return redis.call('hincrby', KEYS[1], ARGV[1], -decrby) end return -1";
//
//
//    public boolean setLock(String key, long expire) {
//        Boolean b = (Boolean) redisTemplate.execute(new RedisCallback<Boolean>() {
//            @Override
//            public Boolean doInRedis(RedisConnection connection) throws DataAccessException {
//                RedisSerializer keySerializer = redisTemplate.getKeySerializer();
//                Object obj = connection.execute("set", keySerializer.serialize(key),
//                        SafeEncoder.encode("NX"),
//                        SafeEncoder.encode("EX"),
//                        Protocol.toByteArray(expire));
//                return obj != null;
//            }
//        });
//        return b;
//    }
//
//    public String getLock(String key) {
//        try {
//            RedisCallback<String> callback = (connection) -> {
//                JedisCommands commands = (JedisCommands) connection.getNativeConnection();
//                return commands.get(key);
//            };
//            Object result = redisTemplate.execute(callback);
//            return String.valueOf(result);
//        } catch (Exception e) {
//            log.error("get redis occured an exception", e);
//        }
//        return "";
//    }
//
//    public boolean releaseLock(String key, String requestId) {
//        // 释放锁的时候，有可能因为持锁之后方法执行时间大于锁的有效期，此时有可能已经被另外一个线程持有锁，所以不能直接删除
//        try {
//            List<String> keys = new ArrayList<>();
//            keys.add(key);
//            List<String> args = new ArrayList<>();
//            args.add(requestId);
//
//            // 使用lua脚本删除redis中匹配value的key，可以避免由于方法执行时间过长而redis锁自动过期失效的时候误删其他线程的锁
//            // spring自带的执行脚本方法中，集群模式直接抛出不支持执行脚本的异常，所以只能拿到原redis的connection来执行脚本
//            RedisCallback<Long> callback = (connection) -> {
//                Object nativeConnection = connection.getNativeConnection();
//                // 集群模式和单机模式虽然执行脚本的方法一样，但是没有共同的接口，所以只能分开执行
//                // 集群模式
//                if (nativeConnection instanceof JedisCluster) {
//                    return (Long) ((JedisCluster) nativeConnection).eval(UNLOCK_LUA, keys, args);
//                }
//
//                // 单机模式
//                else if (nativeConnection instanceof Jedis) {
//                    return (Long) ((Jedis) nativeConnection).eval(UNLOCK_LUA, keys, args);
//                }
//                return 0L;
//            };
//
//            Object result = redisTemplate.execute(callback);
//
//            return result != null && Long.parseLong(result.toString()) > 0;
//        } catch (Exception e) {
//            log.error("release lock occured an exception", e);
//        }
//        return false;
//    }
//
//    /**
//     * 扣库存
//     *
//     * @param key 库存key
//     * @param num 扣减库存数量
//     * @return 扣减之后剩余的库存【 -1:库存不足 大于等于0:扣减库存之后的剩余库存】
//     */
//    public Long deductStock(String name, String key, int num) {
//        // 脚本里的KEYS参数
//        List<String> keys = new ArrayList<>();
//        keys.add(name);
//        // 脚本里的ARGV参数
//        List<String> args = new ArrayList<>();
//        args.add(key);
//        args.add(Integer.toString(num));
//
//        Object result = redisTemplate.execute(new RedisCallback<Long>() {
//            @Override
//            public Long doInRedis(RedisConnection connection) throws DataAccessException {
//                Object nativeConnection = connection.getNativeConnection();
//                // 集群模式和单机模式虽然执行脚本的方法一样，但是没有共同的接口，所以只能分开执行
//                // 集群模式
//                if (nativeConnection instanceof JedisCluster) {
//                    return (Long) ((JedisCluster) nativeConnection).eval(STOCK_LUA, keys, args);
//                }
//
//                // 单机模式
//                else if (nativeConnection instanceof Jedis) {
//                    return (Long) ((Jedis) nativeConnection).eval(STOCK_LUA, keys, args);
//                }
//                return -1L;
//            }
//        });
//        return Long.parseLong(result.toString());
//    }
//}
