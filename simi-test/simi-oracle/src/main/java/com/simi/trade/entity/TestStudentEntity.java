package com.simi.trade.entity;
import com.simi.trade.entity.listener.TestStudentListener;
import lombok.Data;

import jakarta.persistence.*;
import org.hibernate.annotations.GenericGenerator;

@Data
@Entity
@Table(name = "TEST_STUDENT")
@EntityListeners(TestStudentListener.class)
@GenericGenerator(name="stuIdGenerator", strategy = "com.simi.trade.entity.generator.TestStudentIdGenerator")
public class TestStudentEntity {

    @GeneratedValue(generator = "stuIdGenerator")
    @Id
    @Column(name = "STU_ID")
    private Long stuId;

    @Column(name = "PER_ID")
    private Long perId;

    @Column(name = "CLASS")
    private String className;

    @Column(name = "ROLE")
    private StudentRoleEnum role;
}
