package com.simi.sgz.domain;

import com.simi.sgz.RobotAction;

import static com.simi.sgz.RobotAction.*;


public abstract class Operation {
    public Operation(Coordinate red_point, Coordinate stop_btn1, Coordinate confirm, Coordinate dangerous_confirm,
                     Coordinate city_add_troops, Coordinate citi_confirm, Coordinate citi_back,
                     Coordinate army_btn, Coordinate coordinate_btn, Coordinate scroll_top, Coordinate scroll_bot,
                     Coordinate btn1, Coordinate btn2, Coordinate btn3, Coordinate btn4, Coordinate tab1,
                     Coordinate tab2, Coordinate tab3, Coordinate army1_from1, Coordinate army1_from5,
                     Coordinate army2_from5, Coordinate army3_from5, Coordinate army4_from5,Coordinate army5_from5,
                     Coordinate mark1, Coordinate mark2, Coordinate mark3, Coordinate mark4) {
        this.red_point = red_point;
        this.stop_btn1 = stop_btn1;
        this.confirm = confirm;
        this.dangerous_confirm = dangerous_confirm;
        this.city_add_troops = city_add_troops;
        this.citi_confirm = citi_confirm;
        this.citi_back = citi_back;
        this.army_btn = army_btn;
        this.coordinate_btn = coordinate_btn;
        this.scroll_top = scroll_top;
        this.scroll_bot = scroll_bot;
        this.btn1 = btn1;
        this.btn2 = btn2;
        this.btn3 = btn3;
        this.btn4 = btn4;
        this.tab1 = tab1;
        this.tab2 = tab2;
        this.tab3 = tab3;
        this.army1_from1 = army1_from1;
        this.army1_from5 = army1_from5;
        this.army2_from5 = army2_from5;
        this.army3_from5 = army3_from5;
        this.army4_from5 = army4_from5;
        this.army5_from5 = army5_from5;
        this.mark1 = mark1;
        this.mark2 = mark2;
        this.mark3 = mark3;
        this.mark4 = mark4;
    }

    Coordinate red_point;
    Coordinate stop_btn1;
    // Round button
    Coordinate confirm;
    Coordinate dangerous_confirm;

    // main-city  increase the number of troops
    Coordinate city_add_troops;
    // main-city  confirm
    Coordinate citi_confirm;
    Coordinate citi_back;

    // Scroll locations
    Coordinate army_btn;
    Coordinate coordinate_btn;
    Coordinate scroll_top;
    Coordinate scroll_bot;

    // The button menu that appears after clicking
    Coordinate btn1;
    Coordinate btn2;
    Coordinate btn3;
    Coordinate btn4;

    // Right tabs
    Coordinate tab1;
    Coordinate tab2;
    Coordinate tab3;

    // The list of armies below
    Coordinate army1_from1;
    Coordinate army1_from5;
    Coordinate army2_from5;
    Coordinate army3_from5;
    Coordinate army4_from5;
    Coordinate army5_from5;

    // Marked positions starting from the end.
    Coordinate mark1;
    Coordinate mark2;
    Coordinate mark3;
    Coordinate mark4;
    public void enterCity(RobotAction robot){
        robot.leftMouseClick(coordinate_btn);
        robot.scroll(scroll_top,scroll_bot);
        robot.scroll(scroll_top,scroll_bot);
        robot.scroll(scroll_top,scroll_bot);
        robot.scroll(scroll_top,scroll_bot);
        robot.scroll(scroll_top,scroll_bot);
        robot.scroll(scroll_top,scroll_bot);
        robot.leftMouseClick(mark4, wt4);
        robot.leftMouseClick(red_point);
    }


    public void supplyArmy(RobotAction robot, boolean[] failPassedList, boolean[] supplyList){
        for (int i = 0; i < failPassedList.length; i++) {
            if(failPassedList[i])continue;
            if(!supplyList[i])continue;
            robot.leftMouseClick(getArmyLocationInCity(i));
            robot.leftMouseClick(city_add_troops);
            robot.leftMouseClick(citi_confirm);
            robot.leftMouseClick(citi_back);
        }
    }
    public void goBack(RobotAction robot){
        robot.leftMouseClick(citi_back);
        robot.scroll(scroll_bot,scroll_top);
        robot.scroll(scroll_bot,scroll_top);
        robot.scroll(scroll_bot,scroll_top);
        robot.scroll(scroll_bot,scroll_top);
        robot.scroll(scroll_bot,scroll_top);
        robot.scroll(scroll_bot,scroll_top);
    }

    public void clear(RobotAction robot, int mainCityArmyNumber, boolean[] failPassedList, int[] clearMarkList, int[] clearTabList, Integer waitIndex, Integer waitIndex2) {
        for (int i = 0; i < failPassedList.length; i++) {
            if(failPassedList[i])continue;
            int armyIndex=i>=mainCityArmyNumber?i-mainCityArmyNumber:i;
            robot.leftMouseClick(getMrkByIndex(clearMarkList[i]), wt5);
            if(waitIndex!=null&&i==waitIndex){
                try {
                    Thread.sleep(3000);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
            }
            if(waitIndex2!=null&&i==waitIndex2){
                try {
                    Thread.sleep(7000);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
            }
            robot.leftMouseClick(btn4);
            robot.leftMouseClick(getTabByIndex(clearTabList[i]));
            robot.leftMouseClick(getArmyLocationInCity(armyIndex));
            robot.leftMouseClick(confirm, wt7);
            robot.leftMouseClick(dangerous_confirm, wt7);
        }

    }


    Coordinate getArmyLocationInCity(int index){
        switch (index){
            case 0: return army1_from5;
            case 1: return army2_from5;
            case 2: return army3_from5;
            case 3: return army4_from5;
            case 4: return army5_from5;
            default: return null;
        }
    }
    Coordinate getMrkByIndex(int index){
        switch (index){
            case 1: return mark1;
            case 2: return mark2;
            case 3: return mark3;
            case 4: return mark4;
            default: return null;
        }
    }
    Coordinate getTabByIndex(int index){
        switch (index){
            case 1: return tab1;
            case 2: return tab2;
            case 3: return tab3;
            default: return null;
        }
    }
}
