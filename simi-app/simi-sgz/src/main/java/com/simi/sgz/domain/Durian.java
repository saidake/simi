package com.simi.sgz.domain;
import lombok.experimental.SuperBuilder;

@SuperBuilder
public class Durian extends Operation {
    {
        red_point=Coordinate.of(435,833);
        stop_btn1=Coordinate.of(0,0);
        // Round button
        confirm=Coordinate.of(689,929);
        dangerous_confirm=Coordinate.of(359,928);
        // main-city  increase the number of troops
        city_add_troops=Coordinate.of(98,1050);
        // main-city  confirm
        city_confirm=Coordinate.of(767,999);
        city_back=Coordinate.of(821,608);
        // Scroll locations
        army_btn=Coordinate.of(747,648);
        coordinate_btn=Coordinate.of(819,651);
        scroll_top=Coordinate.of(779,674);
        scroll_bot=Coordinate.of(773,797);

        // The button menu that appears after clicking
        btn1=Coordinate.of(592,773);
        btn2=Coordinate.of(593,818);
        btn3=Coordinate.of(593,859);
        btn4=Coordinate.of(596,901);
        // Right tabs
        tab1=Coordinate.of(785,724);
        tab2=Coordinate.of(778,773);
        tab3=Coordinate.of(773,834);

        // The list of armies below
        army1_from1=Coordinate.of(434,1005);
        army1_from5=Coordinate.of(79,1002);
        army2_from5=Coordinate.of(189,987);
        army3_from5=Coordinate.of(434,1005);
        army4_from5=Coordinate.of(591,1003);
        army5_from5=Coordinate.of(779,998);
        // Marked positions starting from the end.
        mark1=Coordinate.of(775,795);
        mark2=Coordinate.of(778,755);
        mark3=Coordinate.of(779,715);
        mark4=Coordinate.of(777,680);
    }
}
