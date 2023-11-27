package com.simi.support.data;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.fasterxml.jackson.datatype.jsr310.deser.InstantDeserializer;
import com.fasterxml.jackson.datatype.jsr310.ser.InstantSerializer;
import lombok.*;

import java.time.Instant;

@NoArgsConstructor
@Data
@AllArgsConstructor
public class Person{
    public String name;
    public Integer age;
    public Toy toy;
    public ColorEnum colorEnum;

    @JsonDeserialize(using = InstantDeserializer.class)
    @JsonSerialize(using = InstantSerializer.class)
    public Instant date;

    public transient Integer transien;

    public final String test="ddd";
}