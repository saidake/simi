package com.simi.service.trade.domain;


import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
@Schema
public class FxcmAccount {
    private String ticket;
    private String symbol;
    private String volume;
    private String openDate;
    private String sold;
    private String bought;
    private String grossPL;
    private String comm;
    private String dividends;
    private String rollover;
    private String adj;
    private String netPL;
    private String openCondition;
    private String createdBy;

    // other fields
    private boolean buy=false;
    private String closeCondition;
    private String closeDate;

}
