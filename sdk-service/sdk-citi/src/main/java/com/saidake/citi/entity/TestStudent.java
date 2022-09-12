package com.saidake.citi.entity;
import lombok.Data;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Data
@Entity
@Table(name = "TEST_STUDENT")
public class TestStudent {
    @Id
    @Column(name = "STU_ID")
    private Long stuId;

    @Column(name = "PER_ID")
    private Long perId;

    @Column(name = "CLASS")
    private String className;

    @Column(name = "ROLE")
    private String role;
}
