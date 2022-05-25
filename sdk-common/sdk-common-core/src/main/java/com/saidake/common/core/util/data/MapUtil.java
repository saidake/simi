package com.saidake.common.core.util.data;

import java.util.HashMap;

public class MapUtil {
    public static <K,T> HashMap<K,T> of(K key, T value,Object...objectList){
        HashMap hashMap=new HashMap();
        if(objectList.length%2!=0){
            throw new RuntimeException("objectList length remainder error");
        }
        hashMap.put(key,value);
        for (int i = 0; i < objectList.length; i+=2) {
            hashMap.put(objectList[i],objectList[i+1]);
        }
        return hashMap;
    }
}
