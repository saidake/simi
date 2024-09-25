package com.simi.getdata;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class AService {
    private List<Integer> sourceData=Arrays.asList(23,32,53,63,75,73,36,674,7474,52525,252);

    /**
     * 根据索引获取sourceData，构建新数组。
     *
     * @param ids   ID数组
     * @return 新数组
     * @throws IndexOutOfBoundsException
     */
    public List<Integer> get(List<Integer> ids) {
        if(ids.size() > 100) return Collections.emptyList();
        List<Integer> resultList=new ArrayList<>();
        for (Integer id : ids) {
            resultList.add(sourceData.get(id));
        }
        return resultList;
    }
}
