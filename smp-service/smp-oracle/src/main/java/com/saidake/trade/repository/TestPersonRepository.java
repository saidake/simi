package com.saidake.trade.repository;
import com.saidake.trade.entity.TestPersonEntity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TestPersonRepository extends JpaRepository<TestPersonEntity,Long> {
    TestPersonEntity findFirstByPerId(Long perId);
}
