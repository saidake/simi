package com.simi.sgz.domain;

import lombok.experimental.SuperBuilder;


@SuperBuilder
public class Grape extends Operation {
    {
        red_point = Coordinate.of(1343,833);
        stop_btn1 = Coordinate.of(0, 0);
        // Round button
        confirm = Coordinate.of(1596,932);
        dangerous_confirm = Coordinate.of(1268,929);

        // Scroll locations
        army_btn = Coordinate.of(1654,652);
        coordinate_btn = Coordinate.of(1732,650);
        scroll_top = Coordinate.of(1686,677);
        scroll_bot = Coordinate.of(1690,801);

        // main-city  increase the number of troops
        city_add_troops = Coordinate.of(1007,1051);
        // main-city  confirm
        city_confirm = Coordinate.of(1674,1001);
        city_back = Coordinate.of(1726,608);

        // The button menu that appears after clicking
        btn1 = Coordinate.of(1499,774);
        btn2 = Coordinate.of(1499,816);
        btn3 = Coordinate.of(1500,859);
        btn4 = Coordinate.of(1503,902);
        // Right tabs
        tab1 = Coordinate.of(1689,726);
        tab2 = Coordinate.of(1690,770);
        tab3 = Coordinate.of(1690,821);

        // The list of armies below
        army1_from1 = Coordinate.of(1334,999);
        army1_from5 = Coordinate.of(998,1002);
        army2_from5 = Coordinate.of(1176,1005);
        army3_from5 = Coordinate.of(1334,999);
        army4_from5 = Coordinate.of(1520,995);
        army5_from5 = Coordinate.of(1689,1003);

        // Marked positions starting from the end.
        mark1 = Coordinate.of(1686,796);
        mark2 = Coordinate.of(1685,754);
        mark3 = Coordinate.of(1684,715);
        mark4 = Coordinate.of(1687,678);
    }

}
