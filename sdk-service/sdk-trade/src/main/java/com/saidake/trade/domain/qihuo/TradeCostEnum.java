package com.saidake.trade.domain.qihuo;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum TradeCostEnum {
    SR(3.01),
    CF(8.62),
    OI(4.02),
    MA(8.02),
    RM(3.02),
    SA(3.51),
    PF(6.02),
    A(4.02 ),
    B(2.02 ),
    C(2.42 ),
    M(3.02),
    Y(5.02),
    L(1.02),
    V(1.02),
    PP(1.02),
    CS(3.02),
    EG(6.02),
    EB(6.02),
    PG(12.02 );
    private Double cost;
}
