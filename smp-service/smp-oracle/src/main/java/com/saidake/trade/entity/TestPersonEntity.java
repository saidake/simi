package com.saidake.trade.entity;
import lombok.Data;

import jakarta.persistence.*;

import java.util.List;

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

    @OneToMany(cascade = CascadeType.ALL,fetch = FetchType.EAGER)
    @JoinColumn(name="PER_ID",referencedColumnName = "PER_ID")
    private List<TestStudentEntity> testStudentEntities;
}
