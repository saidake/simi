package com.simi.sgz.domain;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

@SuperBuilder
public class Orange extends Operation {
    {
        red_point = Coordinate.of(1340,306);
        stop_btn1 = Coordinate.of(0, 0);
        // Round button
        confirm = Coordinate.of(1591,401);
        dangerous_confirm = Coordinate.of(1265,403);

        // Scroll locations
        army_btn = Coordinate.of(1652,122);
        coordinate_btn = Coordinate.of(1728,123);
        scroll_top = Coordinate.of(1685,154);
        scroll_bot = Coordinate.of(1686,276);

        // main-city  increase the number of troops
        city_add_troops = Coordinate.of(1008,523);
        // main-city  confirm
        city_confirm = Coordinate.of(1667,467);
        city_back = Coordinate.of(1727,82);

        // The button menu that appears after clicking
        btn1 = Coordinate.of(1500,248);
        btn2 = Coordinate.of(1495,292);
        btn3 = Coordinate.of(1499,333);
        btn4 = Coordinate.of(1500,377);
        // Right tabs
        tab1 = Coordinate.of(1684,200);
        tab2 = Coordinate.of(1690,249);
        tab3 = Coordinate.of(1681,305);

        // The list of armies below
        army1_from1 = Coordinate.of(1345,477);
        army1_from5 = Coordinate.of(988,475);
        army2_from5 = Coordinate.of(1167,477);
        army3_from5 = Coordinate.of(1338,475);
        army4_from5 = Coordinate.of(1514,477);
        army5_from5 = Coordinate.of(1695,477);

        // Marked positions starting from the end.
        mark1 = Coordinate.of(1681,270);
        mark2 = Coordinate.of(1681,229);
        mark3 = Coordinate.of(1687,191);
        mark4 = Coordinate.of(1683,155);
    }

}
