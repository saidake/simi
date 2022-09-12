package com.saidake.generator.mybatis.entity;

import lombok.Data;

@Data
public class ColumnEntity {
    private String columnName;
    private String nullable;
    private String dataType;
    private String columnComment;

}
