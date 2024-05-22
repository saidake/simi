package com.simi.sgz;

import lombok.AllArgsConstructor;

@AllArgsConstructor
public enum Coordinate {
    // ============================== Pineapple
    PI_RED_POINT(284, 569),
    PI_BT1(473,496),
    PI_BT2(466, 543),
    PI_STOP_BT1(502,775),

    PI_TAB1(440,302),
    PI_TAB2(442,365),
    PI_TAB3(443,415),

    PI_ARMY1_FROM1(279,849),
    PI_CONFIRM(453,911),
    PI_SMALL_CONFIRM(196,668),

    PI_MARK1(458,391 ),
    PI_MARK2(460,341 ),
    PI_MARK3(458,298 ),
    // ============================== Pumpkin
    PU_RED_POINT(899, 573),
    PU_BT1(1076,498),
    PU_BT2(1088,542),
    PU_STOP_BT1(1114,778),

    PU_TAB1(1058,293),
    PU_TAB2(1062,364),
    PU_TAB3(1063,419),
    PU_ARMY1_FROM1(891,840),
    PU_ARMY2_FROM2(943,842),
    PU_CONFIRM(1068,911),
    PU_SMALL_CONFIRM(807,668),

    PU_MARK1(1072,388 ),
    PU_MARK2(1071,344 ),
    PU_MARK3(1075,301 ),

    // ============================== Durian
    DU_RED_POINT(1511, 567),
    DU_BT1(1695,494),
    DU_BT2(1694,538),
    DU_STOP_BT1(1726,773),

    DU_TAB1(1675,291),
    DU_TAB2(1678,362),
    DU_TAB3(1675,417),

    DU_ARMY1_FROM1(1504,838),
    DU_CONFIRM(1681,907),
    DU_SMALL_CONFIRM(1424,666),
    DU_MARK1(1683, 387 ),
    DU_MARK2(1684, 343 ),
    DU_MARK3(1684, 297 );

    int x;
    int y;
}
