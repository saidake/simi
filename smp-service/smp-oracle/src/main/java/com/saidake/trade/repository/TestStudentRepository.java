package com.saidake.trade.repository;
import com.saidake.trade.entity.TestStudentEntity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TestStudentRepository extends JpaRepository<TestStudentEntity,Long> {

    TestStudentEntity findFirstByStuId(Long stuId);
}
