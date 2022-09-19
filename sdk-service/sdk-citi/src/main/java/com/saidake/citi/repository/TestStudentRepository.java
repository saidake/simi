package com.saidake.citi.repository;
import com.saidake.citi.entity.TestStudentEntity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TestStudentRepository extends JpaRepository<TestStudentEntity,Long> {

    TestStudentEntity findFirstByStuId(Long stuId);
}
