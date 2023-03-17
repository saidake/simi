package com.saidake.test.data;

import lombok.*;
import lombok.extern.jackson.Jacksonized;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class Person{
    public String name;
    public Integer age;
    public Toy toy;
    public transient Integer transien;
    public final String test="ddd";

}