package com.simi.sgz.domain;

import lombok.AllArgsConstructor;

@AllArgsConstructor

public enum orange implements Coordinate{
    red_point(899, 573),
    stop_btn1(1114,778),
    small_confirm(807,668),
    // Round button
    confirm(1068,911),
    dangerous_confirm(810,679),

    // Scroll locations
    army_btn(1037,222),
    coordinate_btn(1112,220),
    scroll_top(1064,260),
    scroll_bot(1064,360),

    // main-city  increase the number of troops
    city_add_troops(698,873),
    // main-city  confirm
    citi_confirm(891,958),
    citi_back(704,1042),

    // The button menu that appears after clicking
    btn1(1076,498),
    btn2(1088,542),
    btn3(1088,586),
    btn4(1088,630),
    // Right tabs
    tab1(1058,293),
    tab2(1062,364),
    tab3(1063,419),

    // The list of armies below
    army1_from1(891,840),
    army2_from2(943,842),
    army1_from5(666,839),
    army2_from5(785,839),
    army3_from5(894,839),
    army4_from5(1006,839),

    // Marked positions starting from the end.
    mark1(1072,388 ),
    mark2(1071,344 ),
    mark3(1075,301 ),
    mark4(1075,254 );

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
