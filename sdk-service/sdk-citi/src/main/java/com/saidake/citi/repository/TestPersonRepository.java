package com.saidake.citi.repository;
import com.saidake.citi.entity.TestPersonEntity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TestPersonRepository extends JpaRepository<TestPersonEntity,Long> {
    TestPersonEntity findFirstByPerId(Long perId);
}
