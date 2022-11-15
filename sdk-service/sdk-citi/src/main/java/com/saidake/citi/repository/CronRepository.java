package com.saidake.citi.repository;

import com.saidake.citi.entity.CronEntity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface CronRepository extends JpaRepository<CronEntity, Long> {
    CronEntity findFirstByCronName(String cronName);
}
