package com.saidake.common.util.file.support.yaml;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.stream.Collectors;

public class StringList extends ArrayList<String> {
    public StringList(String source){
        super();
        String[] split = source.split(",");
        this.addAll(Arrays.stream(split).collect(Collectors.toList()));
    }
}
