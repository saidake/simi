package com.simi.trade.entity;


import lombok.Data;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.util.Date;

@Data
@Entity
@Table(name = "CRON")
public class CronEntity {
    @Id
    @Column(name = "CRON_ID")
    private Long cronId;

    @Column(name = "CRON")
    private String cron;

    @Column(name = "CRON_NAME")
    private String cronName;

    @Column(name = "ISRT_DT")
    private Date isrtDt;

    @Column(name = "EXECUTE_TIME")
    private String executeTime;

    @Column(name = "EXECUTE_BY")
    private String executeBy;

    @Column(name = "LOCKED")
    private Boolean locked;
}
