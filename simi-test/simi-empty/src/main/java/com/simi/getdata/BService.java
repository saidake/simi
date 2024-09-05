package com.simi.getdata;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class BService {
    AService aService=new AService();
    public List<Integer> get(List<Integer> ids) {
        //A. 检测ID数组长度
        if(ids.size() > 100) return Collections.emptyList();
        //A. 获取结果
        List<Integer> resultList=new ArrayList<>();
        for (int i = 0,j=10; i < ids.size(); i+=10,j+=10) {
            resultList.addAll(aService.get(ids.subList(i, Math.min(j, ids.size()))));
        }
        return resultList;
    }
}
