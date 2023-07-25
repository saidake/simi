package com.simi.trade.repository;

import com.simi.trade.entity.CronEntity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface CronRepository extends JpaRepository<CronEntity, Long> {
    CronEntity findFirstByCronName(String cronName);
}
