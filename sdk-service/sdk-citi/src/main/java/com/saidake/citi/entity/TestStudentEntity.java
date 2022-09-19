package com.saidake.citi.entity;
import com.saidake.citi.entity.listener.TestStudentListener;
import lombok.Data;

import javax.persistence.*;

@Data
@Entity
@Table(name = "TEST_STUDENT")
@EntityListeners(TestStudentListener.class)
public class TestStudentEntity {
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
