package com.simi.sgz.domain;

import lombok.AllArgsConstructor;

@AllArgsConstructor

public enum durian implements Coordinate{
    red_point(1511, 567),
    stop_btn1(1726,773),
    small_confirm(1424,666),
    // Round button
    confirm(1681,907),
    dangerous_confirm(1421,679),
    // main-city  increase the number of troops
    city_add_troops(1310,870),
    // main-city  confirm
    citi_confirm(1509,962),
    citi_back(1318,1044),


    // Scroll locations
    army_btn(1647,219),
    coordinate_btn(1738,220),
    scroll_top(1687,260),
    scroll_bot(1687,360),

    // The button menu that appears after clicking
    btn1(1695,494),
    btn2(1694,538),
    btn3(1694,585),
    btn4(1694,631),
    // Right tabs
    tab1(1675,291),
    tab2(1678,362),
    tab3(1675,417),

    // The list of armies below
    army1_from1(1504,838),
    army1_from5(1283,843),
    army2_from5(1397,843),
    army3_from5(1506,843),
    army4_from5(1621,843),


    // Marked positions starting from the end.
    mark1(1683, 387 ),
    mark2(1684, 343 ),
    mark3(1684, 297 ),
    mark4(1684, 257 );

    int x;
    int y;

    @Override
    public int getX() {
        return x;
    }

    @Override
    public int getY() {
        return y;
    }
}
