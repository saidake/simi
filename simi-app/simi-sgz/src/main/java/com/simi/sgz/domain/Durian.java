package com.simi.sgz.domain;



public class Durian extends Operation{
    static Coordinate red_point=Coordinate.of(1511, 567);
    static Coordinate stop_btn1=Coordinate.of(1726,773);
    static Coordinate small_confirm=Coordinate.of(1424,666);
    // Round button
    static Coordinate confirm=Coordinate.of(1681,907);
    static Coordinate dangerous_confirm=Coordinate.of(1421,679);
    // main-city  increase the number of troops
    static Coordinate city_add_troops=Coordinate.of(1310,870);
    // main-city  confirm
    static Coordinate citi_confirm=Coordinate.of(1509,962);
    static Coordinate citi_back=Coordinate.of(1318,1044);


    // Scroll locations
    static Coordinate army_btn=Coordinate.of(1647,219);
    static Coordinate coordinate_btn=Coordinate.of(1738,220);
    static Coordinate scroll_top=Coordinate.of(1687,260);
    static Coordinate scroll_bot=Coordinate.of(1687,360);

    // The button menu that appears after clicking
    static Coordinate btn1=Coordinate.of(1695,494);
    static Coordinate btn2=Coordinate.of(1694,538);
    static Coordinate btn3=Coordinate.of(1694,585);
    static Coordinate btn4=Coordinate.of(1694,634);
    // Right tabs
    static Coordinate tab1=Coordinate.of(1675,291);
    static Coordinate tab2=Coordinate.of(1678,362);
    static Coordinate tab3=Coordinate.of(1675,417);

    // The list of armies below
    static Coordinate army1_from1=Coordinate.of(1504,838);
    static Coordinate army1_from5=Coordinate.of(1283,843);
    static Coordinate army2_from5=Coordinate.of(1397,843);
    static Coordinate army3_from5=Coordinate.of(1506,843);
    static Coordinate army4_from5=Coordinate.of(1621,843);
    static Coordinate army5_from5=Coordinate.of(1724,843);


    // Marked positions starting from the end.
    static Coordinate mark1=Coordinate.of(1683, 387 );
    static Coordinate mark2=Coordinate.of(1684, 343 );
    static Coordinate mark3=Coordinate.of(1684, 297 );
    static Coordinate mark4=Coordinate.of(1684, 257 );


    public Durian(){
        super(
                red_point,
                stop_btn1,
                confirm,
                dangerous_confirm,
                city_add_troops,
                citi_confirm,
                citi_back,
                army_btn,
                coordinate_btn,
                scroll_top,
                scroll_bot,
                btn1,
                btn2,
                btn3,
                btn4,
                tab1,
                tab2,
                tab3,
                army1_from1,
                army1_from5,
                army2_from5,
                army3_from5,
                army4_from5,
                army5_from5,
                mark1,
                mark2,
                mark3,
                mark4
        );
    }
}
