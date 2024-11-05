package com.simi.generator.model.properties;

import lombok.Data;

import java.util.List;


@Data
public  class DbConfig{
    private List<String> passFields;
    private List<String> passTables;
    private List<String> generateTables;
    private List<String> requestPassFields;
    private List<String> controllerPassTables;
}
