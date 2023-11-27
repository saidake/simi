package com.simi;


import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import java.util.Map;

public class Main {
    public static void main(String[] args) throws JsonProcessingException {
        Map<String, Integer> map = new ObjectMapper().readValue("{\"a\":999}", Map.class);
        System.out.println(map);
    }
}