package com.simi.sgz.domain;

import lombok.AllArgsConstructor;

@AllArgsConstructor

public enum pineapple implements Coordinate{
    red_point(284, 569),
    stop_btn1(502,775),
    // Round button
    confirm(451,907),
    dangerous_confirm(195,682),

    // main-city  increase the number of troops
    city_add_troops(79,875),
    // main-city  confirm
    citi_confirm(283,962),
    citi_back(95,1044),

    // Scroll locations
    army_btn(423,219),
    coordinate_btn(512,220),
    scroll_top(464,260),
    scroll_bot(464,360),

    // The button menu that appears after clicking
    btn1(473,496),
    btn2(466, 543),
    btn3(466, 587),
    btn4(466, 632),

    // Right tabs
    tab1(440,302),
    tab2(442,365),
    tab3(443,415),

    // The list of armies below
    army1_from1(279,849),
    army1_from5(57,849),
    army2_from5(165,849),
    army3_from5(280,849),
    army4_from5(400,849),

    // Marked positions starting from the end.
    mark1(458,391 ),
    mark2(460,341 ),
    mark3(458,298 ),
    mark4(458,254 );

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
