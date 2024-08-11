package com.simi.sgz.domain;


public class Pineapple extends Operation{
    static Coordinate red_point=Coordinate.of(284, 569);
    static Coordinate stop_btn1=Coordinate.of(502,775);
    // Round button
    static Coordinate confirm=Coordinate.of(451,907);
    static Coordinate dangerous_confirm=Coordinate.of(195,682);

    // main-city  increase the number of troops
    static Coordinate city_add_troops=Coordinate.of(79,875);
    // main-city  confirm
    static Coordinate citi_confirm=Coordinate.of(283,962);
    static Coordinate citi_back=Coordinate.of(95,1044);

    // Scroll locations
    static Coordinate army_btn=Coordinate.of(423,219);
    static Coordinate coordinate_btn=Coordinate.of(512,220);
    static Coordinate scroll_top=Coordinate.of(464,260);
    static Coordinate scroll_bot=Coordinate.of(464,360);

    // The button menu that appears after clicking
    static Coordinate btn1=Coordinate.of(473,496);
    static Coordinate btn2=Coordinate.of(466, 543);
    static Coordinate btn3=Coordinate.of(466, 587);
    static Coordinate btn4=Coordinate.of(466, 632);

    // Right tabs
    static Coordinate tab1=Coordinate.of(440,302);
    static Coordinate tab2=Coordinate.of(442,365);
    static Coordinate tab3=Coordinate.of(443,415);

    // The list of armies below
    static Coordinate army1_from1=Coordinate.of(279,849);
    static Coordinate army1_from5=Coordinate.of(57,849);
    static Coordinate army2_from5=Coordinate.of(165,849);
    static Coordinate army3_from5=Coordinate.of(280,849);
    static Coordinate army4_from5=Coordinate.of(400,849);
    static Coordinate army5_from5=Coordinate.of(497,849);

    // Marked positions starting from the end.
    static Coordinate mark1=Coordinate.of(458,391 );
    static Coordinate mark2=Coordinate.of(460,341 );
    static Coordinate mark3=Coordinate.of(458,298 );
    static Coordinate mark4=Coordinate.of(458,254 );

    public Pineapple(){
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
