package com.simi.sgz.domain;

import com.simi.sgz.AAAconfig.WaitingTime;
import com.simi.sgz.RobotAction;
import lombok.experimental.SuperBuilder;


@SuperBuilder
public abstract class Operation {
    Coordinate red_point;
    Coordinate stop_btn1;
    // Round button
    Coordinate confirm;
    Coordinate dangerous_confirm;

    // main-city  increase the number of troops
    Coordinate city_add_troops;
    // main-city  confirm
    Coordinate city_confirm;
    Coordinate city_back;

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
        robot.leftMouseClick(mark4, WaitingTime.SELECT_MARK);
        robot.leftMouseClick(red_point, WaitingTime.ENTER_CITY);
    }


    public void supplyArmy(RobotAction robot, int mainCityArmyNumber, int armyIndex){
        robot.leftMouseClick(getArmyLocationInCity(armyIndex<mainCityArmyNumber?armyIndex:armyIndex-mainCityArmyNumber));
        robot.leftMouseClick(city_add_troops);
        robot.leftMouseClick(city_confirm);
        robot.leftMouseClick(city_back);
    }
    public void goBackAndScrollToBottom(RobotAction robot){
        robot.leftMouseClick(city_back);
        robot.scroll(scroll_bot,scroll_top);
        robot.scroll(scroll_bot,scroll_top);
        robot.scroll(scroll_bot,scroll_top);
        robot.scroll(scroll_bot,scroll_top);
        robot.scroll(scroll_bot,scroll_top);
        robot.scroll(scroll_bot,scroll_top);
    }

    public void clear(RobotAction robot, int mainCityArmyNumber, int curIndex, int markIndex, int clearTabIndex) {
        robot.leftMouseClick(getMrkByIndex(markIndex), WaitingTime.SELECT_MARK);
        robot.leftMouseClick(btn4);
        robot.leftMouseClick(getTabByIndex(clearTabIndex));
        robot.leftMouseClick(getArmyLocationInCity(curIndex<mainCityArmyNumber?curIndex:curIndex-mainCityArmyNumber), WaitingTime.SELECT_TROOP_IN_CITY);
        robot.leftMouseClick(confirm, WaitingTime.CONFIRM);
        robot.leftMouseClick(dangerous_confirm, WaitingTime.CONFIRM);
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
