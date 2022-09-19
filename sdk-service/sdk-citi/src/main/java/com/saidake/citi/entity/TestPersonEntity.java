package com.saidake.citi.entity;
import com.saidake.citi.entity.listener.TestStudentListener;
import lombok.Data;

import javax.persistence.*;

@Data
@Entity
@Table(name = "TEST_PERSON")
public class TestPersonEntity {
    @Id
    @Column(name = "PER_ID")
    private Long perId;

    @Column(name = "AGE")
    private Long age;

    @Column(name = "NAME")
    private String name;

    @Column(name = "GENDER")
    private Integer gender;
}
