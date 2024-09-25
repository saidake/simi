package com.simi.common.test.pojo;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class Person {
    public String name;
    public Integer age;

    @JsonFormat(shape = JsonFormat.Shape.STRING)
    public Gender gender;
    public enum Gender{
        FEMALE, MALE
    }
}
