package com.simi.sgz.domain;

import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

public class Orange extends Operation{

    static Coordinate red_point=Coordinate.of(899, 573);
    static Coordinate stop_btn1=Coordinate.of(1114,778);
    static Coordinate small_confirm=Coordinate.of(807,668);
    // Round button
    static Coordinate confirm=Coordinate.of(1068,911);
    static Coordinate dangerous_confirm=Coordinate.of(810,679);

    // Scroll locations
    static Coordinate army_btn=Coordinate.of(1037,222);
    static Coordinate coordinate_btn=Coordinate.of(1112,220);
    static Coordinate scroll_top=Coordinate.of(1064,260);
    static Coordinate scroll_bot=Coordinate.of(1064,360);

   // main-city  increase the number of troops
    static Coordinate city_add_troops=Coordinate.of(698,873);
    // main-city  confirm
    static Coordinate citi_confirm=Coordinate.of(891,958);
    static Coordinate citi_back=Coordinate.of(704,1042);

    // The button menu that appears after clicking
    static Coordinate btn1=Coordinate.of(1076,498);
    static Coordinate btn2=Coordinate.of(1088,542);
    static Coordinate btn3=Coordinate.of(1088,586);
    static Coordinate btn4=Coordinate.of(1088,630);
    // Right tabs
    static Coordinate tab1=Coordinate.of(1058,293);
    static Coordinate tab2=Coordinate.of(1062,364);
    static Coordinate tab3=Coordinate.of(1063,419);

    // The list of armies below
    static Coordinate army1_from1=Coordinate.of(891,840);
    static Coordinate army2_from2=Coordinate.of(943,842);
    static Coordinate army1_from5=Coordinate.of(666,839);
    static Coordinate army2_from5=Coordinate.of(785,839);
    static Coordinate army3_from5=Coordinate.of(894,839);
    static Coordinate army4_from5=Coordinate.of(1006,839);
    static Coordinate army5_from5=Coordinate.of(1120,843);

    // Marked positions starting from the end.
    static Coordinate mark1=Coordinate.of(1072,388 );
    static Coordinate mark2=Coordinate.of(1071,344 );
    static Coordinate mark3=Coordinate.of(1075,301 );
    static Coordinate mark4=Coordinate.of(1075,254 );
    public Orange(){
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
