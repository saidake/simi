package com.saidake.trade.repository;

import com.saidake.trade.entity.CronEntity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface CronRepository extends JpaRepository<CronEntity, Long> {
    CronEntity findFirstByCronName(String cronName);
}
