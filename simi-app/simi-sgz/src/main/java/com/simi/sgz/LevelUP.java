package com.simi.sgz;

import com.simi.sgz.domain.pineapple;
import com.simi.sgz.domain.orange;
import com.simi.sgz.domain.durian;

import java.awt.*;

import static com.simi.sgz.RobotAction.wt4;
import static com.simi.sgz.RobotAction.wt5;

public class LevelUP {
    public static void main(String[] args) throws AWTException {
        RobotAction robot = new RobotAction();
        int a=106-15, b=61-15, c=61-15, d=61-15, pinTime=63+30;
        int e=106-15, f=61-15, g=61-15, h=106-15, oraTime=93+45;
        int i=91-15, j=46-15, k=46-15, l=46-15, durTime=153+75;
        while (a>0||b>0||c>0||d>0){
            a-=15;
            b-=15;
            c-=15;
            d-=15;
            e-=15;
            f-=15;
            g-=15;
            h-=15;
            i-=15;
            j-=15;
            k-=15;
            l-=15;
            boolean pin=a>0||b>0||c>0||d>0;
            boolean ora=e>0||f>0||g>0||h>0;
            boolean dur=i>0||j>0||k>0||l>0;
            if(pin)pineapple(robot,a>0,b>0,c>0,d>0);
            if(ora)orange(robot,e>0,f>0,g>0,h>0);
            if(dur)durian(robot,i>0,j>0,k>0,l>0);
            robot.sleep(Math.max(Math.max(pin?pinTime:0,ora?oraTime:0),dur?durTime:0)*1000);
        }
    }

    private static void pineapple(RobotAction robot, boolean army1, boolean army2, boolean army3, boolean army4) {
        robot.leftMouseClick(pineapple.coordinate_btn);
        robot.scroll(pineapple.scroll_top,pineapple.scroll_bot);
        robot.scroll(pineapple.scroll_top,pineapple.scroll_bot);
        robot.scroll(pineapple.scroll_top,pineapple.scroll_bot);
        robot.leftMouseClick(pineapple.mark4, wt4);
        robot.leftMouseClick(pineapple.red_point);
        if(army1){
            robot.leftMouseClick(pineapple.army1_from5);
            robot.leftMouseClick(pineapple.city_add_troops);
            robot.leftMouseClick(pineapple.citi_confirm);
            robot.leftMouseClick(pineapple.citi_back);
        }
        if(army2){
            robot.leftMouseClick(pineapple.army2_from5);
            robot.leftMouseClick(pineapple.city_add_troops);
            robot.leftMouseClick(pineapple.citi_confirm);
            robot.leftMouseClick(pineapple.citi_back);
        }
//        if(army3){
//            robot.leftMouseClick(pineapple.army3_from5);
//            robot.leftMouseClick(pineapple.city_add_troops);
//            robot.leftMouseClick(pineapple.citi_confirm);
//            robot.leftMouseClick(pineapple.citi_back);
//        }
//        if(army4){
//            robot.leftMouseClick(pineapple.army4_from5);
//            robot.leftMouseClick(pineapple.city_add_troops);
//            robot.leftMouseClick(pineapple.citi_confirm);
//            robot.leftMouseClick(pineapple.citi_back);
//        }
        robot.leftMouseClick(pineapple.citi_back);
        robot.scroll(pineapple.scroll_bot,pineapple.scroll_top);
        robot.scroll(pineapple.scroll_bot,pineapple.scroll_top);
        robot.scroll(pineapple.scroll_bot,pineapple.scroll_top);

        if(army1){
            robot.leftMouseClick(pineapple.mark1, wt4);
            robot.leftMouseClick(pineapple.btn4);
            robot.leftMouseClick(pineapple.army1_from5);
            robot.leftMouseClick(pineapple.confirm, wt5);
            robot.leftMouseClick(pineapple.dangerous_confirm, wt5);
        }
        if(army2){
            robot.leftMouseClick(pineapple.mark2, wt4);
            robot.leftMouseClick(pineapple.btn4);
            robot.leftMouseClick(pineapple.army2_from5);
            robot.leftMouseClick(pineapple.confirm, wt5);
            robot.leftMouseClick(pineapple.dangerous_confirm, wt5);
        }
        if(army3){
            robot.leftMouseClick(pineapple.mark3, wt4);
            robot.leftMouseClick(pineapple.btn4);
            robot.leftMouseClick(pineapple.army3_from5);
            robot.leftMouseClick(pineapple.confirm, wt5);
            robot.leftMouseClick(pineapple.dangerous_confirm, wt5);
        }
        if(army4){
            robot.leftMouseClick(pineapple.mark3, wt4);
            robot.leftMouseClick(pineapple.btn4);
            robot.leftMouseClick(pineapple.army4_from5);
            robot.leftMouseClick(pineapple.confirm, wt5);
            robot.leftMouseClick(pineapple.dangerous_confirm, wt5);
        }
    }

    private static void orange(RobotAction robot, boolean army1, boolean army2, boolean army3, boolean army4) {
        robot.leftMouseClick(orange.coordinate_btn);
        robot.scroll(orange.scroll_top,orange.scroll_bot);
        robot.scroll(orange.scroll_top,orange.scroll_bot);
        robot.scroll(orange.scroll_top,orange.scroll_bot);
        robot.leftMouseClick(orange.mark4, wt4);
        robot.leftMouseClick(orange.red_point);
        if(army1){
            robot.leftMouseClick(orange.army1_from5);
            robot.leftMouseClick(orange.city_add_troops);
            robot.leftMouseClick(orange.citi_confirm);
            robot.leftMouseClick(orange.citi_back);
        }
//        if(army2){
//            robot.leftMouseClick(orange.army2_from5);
//            robot.leftMouseClick(orange.city_add_troops);
//            robot.leftMouseClick(orange.citi_confirm);
//            robot.leftMouseClick(orange.citi_back);
//        }
//        if(army3){
//            robot.leftMouseClick(orange.army3_from5);
//            robot.leftMouseClick(orange.city_add_troops);
//            robot.leftMouseClick(orange.citi_confirm);
//            robot.leftMouseClick(orange.citi_back);
//        }
        robot.leftMouseClick(orange.citi_back);
        robot.scroll(orange.scroll_bot,orange.scroll_top);
        robot.scroll(orange.scroll_bot,orange.scroll_top);
        robot.scroll(orange.scroll_bot,orange.scroll_top);

        if(army1){
            robot.leftMouseClick(orange.mark1, wt4);
            robot.leftMouseClick(orange.btn4);
            robot.leftMouseClick(orange.army1_from5);
            robot.leftMouseClick(orange.confirm, wt5);
            robot.leftMouseClick(orange.dangerous_confirm, wt5);
        }

        if(army2){
            robot.leftMouseClick(orange.mark3, wt4);
            robot.leftMouseClick(orange.btn4);
            robot.leftMouseClick(orange.army2_from5);
            robot.leftMouseClick(orange.confirm, wt5);
            robot.leftMouseClick(orange.dangerous_confirm, wt5);
        }
        if(army3){
            robot.leftMouseClick(orange.mark4, wt4);
            robot.leftMouseClick(orange.btn4);
            robot.leftMouseClick(orange.army3_from5);
            robot.leftMouseClick(orange.confirm, wt5);
            robot.leftMouseClick(orange.dangerous_confirm, wt5);
        }
        if(army4){
            robot.leftMouseClick(orange.mark4, wt4);
            robot.leftMouseClick(orange.btn4);
            robot.leftMouseClick(orange.army4_from5);
            robot.leftMouseClick(orange.confirm, wt5);
            robot.leftMouseClick(orange.dangerous_confirm, wt5);
        }
    }


    private static void durian(RobotAction robot, boolean army1, boolean army2, boolean army3, boolean army4) {
        robot.leftMouseClick(durian.coordinate_btn);
        robot.scroll(durian.scroll_top,durian.scroll_bot);
        robot.scroll(durian.scroll_top,durian.scroll_bot);
        robot.scroll(durian.scroll_top,durian.scroll_bot);
        robot.leftMouseClick(durian.mark4, wt4);
        robot.leftMouseClick(durian.red_point);
        if(army1){
            robot.leftMouseClick(durian.army1_from5);
            robot.leftMouseClick(durian.city_add_troops);
            robot.leftMouseClick(durian.citi_confirm);
            robot.leftMouseClick(durian.citi_back);
        }
//        if(army2){
//            robot.leftMouseClick(durian.army2_from5);
//            robot.leftMouseClick(durian.city_add_troops);
//            robot.leftMouseClick(durian.citi_confirm);
//            robot.leftMouseClick(durian.citi_back);
//        }
//        if(army3){
//            robot.leftMouseClick(durian.army3_from5);
//            robot.leftMouseClick(durian.city_add_troops);
//            robot.leftMouseClick(durian.citi_confirm);
//            robot.leftMouseClick(durian.citi_back);
//        }

        robot.leftMouseClick(durian.citi_back);
        robot.scroll(durian.scroll_bot,durian.scroll_top);
        robot.scroll(durian.scroll_bot,durian.scroll_top);
        robot.scroll(durian.scroll_bot,durian.scroll_top);

        if(army1){
            robot.leftMouseClick(durian.mark1, wt4);
            robot.leftMouseClick(durian.btn4);
            robot.leftMouseClick(durian.army1_from5);
            robot.leftMouseClick(durian.confirm, wt5);
            robot.leftMouseClick(durian.dangerous_confirm, wt5);
        }
        if(army2){
            robot.leftMouseClick(durian.mark2, wt4);
            robot.leftMouseClick(durian.btn4);
            robot.leftMouseClick(durian.army2_from5);
            robot.leftMouseClick(durian.confirm, wt5);
            robot.leftMouseClick(durian.dangerous_confirm, wt5);
        }
        if(army3){
            robot.leftMouseClick(durian.mark3, wt4);
            robot.leftMouseClick(durian.btn4);
            robot.leftMouseClick(durian.army3_from5);
            robot.leftMouseClick(durian.confirm, wt5);
            robot.leftMouseClick(durian.dangerous_confirm, wt5);
        }
        if(army4){
            robot.leftMouseClick(durian.mark4, wt4);
            robot.leftMouseClick(durian.btn4);
            robot.leftMouseClick(durian.army4_from5);
            robot.leftMouseClick(durian.confirm, wt5);
            robot.leftMouseClick(durian.dangerous_confirm, wt5);
        }
    }
}
