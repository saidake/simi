package com.saidake.citi.domain.test;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.Date;

@Data
@Schema(name = "test request body")
public class TestRequestBody {
//    @JsonFormat(shape = JsonFormat.Shape.STRING,pattern = "MM/dd/yyyy HH:mm:ss")
    @JsonFormat(shape = JsonFormat.Shape.STRING,pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(type = "string",example = "2022-02-01 16:53:33")
    private Date date;

    @Schema(example = "zhangsan")
    private String name;
}
