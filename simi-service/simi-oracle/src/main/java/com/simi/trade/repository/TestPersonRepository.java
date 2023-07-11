package com.simi.trade.repository;
import com.simi.trade.entity.TestPersonEntity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TestPersonRepository extends JpaRepository<TestPersonEntity,Long> {
    TestPersonEntity findFirstByPerId(Long perId);
}
