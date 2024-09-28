package com.simi.sgz.domain;

import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

@SuperBuilder
public class Pineapple extends Operation {
    {
        red_point = Coordinate.of(435,312);
        stop_btn1 = Coordinate.of(0, 0);
        // Round button
        confirm = Coordinate.of(689,400);
        dangerous_confirm = Coordinate.of(360,403);

        // main-city  increase the number of troops
        city_add_troops = Coordinate.of(99,523);
        // main-city  confirm
        city_confirm = Coordinate.of(760,470);
        city_back = Coordinate.of(817,82);

        // Scroll locations
        army_btn = Coordinate.of(744,124);
        coordinate_btn = Coordinate.of(823,124);
        scroll_top = Coordinate.of(785,154);
        scroll_bot = Coordinate.of(777,278);

        // The button menu that appears after clicking
        btn1 = Coordinate.of(590,247);
        btn2 = Coordinate.of(596,290);
        btn3 = Coordinate.of(596,335);
        btn4 = Coordinate.of(594,421);

        // Right tabs to select troop group
        tab1 = Coordinate.of(780,196);
        tab2 = Coordinate.of(782,247);
        tab3 = Coordinate.of(776,313);

        // The list of armies below
        army1_from1 = Coordinate.of(442,477);
        army1_from5 = Coordinate.of(79,477);
        army2_from5 = Coordinate.of(263,477);
        army3_from5 = Coordinate.of(434,479);
        army4_from5 = Coordinate.of(605,475);
        army5_from5 = Coordinate.of(782,484);

        // Marked positions starting from the end.
        mark1 = Coordinate.of(773,267);
        mark2 = Coordinate.of(785,229);
        mark3 = Coordinate.of(776,192);
        mark4 = Coordinate.of(772,154);
    }
}
