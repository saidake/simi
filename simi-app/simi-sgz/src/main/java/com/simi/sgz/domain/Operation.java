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
        robot.leftMouseClickEx(coordinate_btn);
        robot.scroll(scroll_top,scroll_bot);
        robot.scroll(scroll_top,scroll_bot);
        robot.scroll(scroll_top,scroll_bot);
        robot.scroll(scroll_top,scroll_bot);
        robot.scroll(scroll_top,scroll_bot);
        robot.scroll(scroll_top,scroll_bot);
        robot.leftMouseClickEx(mark4, WaitingTime.SELECT_MARK);
        robot.leftMouseClickEx(red_point, WaitingTime.ENTER_CITY);
    }


    public void supplyArmy(RobotAction robot, int mainCityArmyNumber, int armyIndex){
        robot.leftMouseClickEx(getArmyLocationInCity(armyIndex<mainCityArmyNumber?armyIndex:armyIndex-mainCityArmyNumber));
        robot.leftMouseClickEx(city_add_troops);
        robot.leftMouseClickEx(city_confirm);
        robot.leftMouseClickEx(city_back);
    }
    public void goBackAndScrollToBottom(RobotAction robot){
        robot.leftMouseClickEx(city_back);
        robot.scroll(scroll_bot,scroll_top);
        robot.scroll(scroll_bot,scroll_top);
        robot.scroll(scroll_bot,scroll_top);
        robot.scroll(scroll_bot,scroll_top);
        robot.scroll(scroll_bot,scroll_top);
        robot.scroll(scroll_bot,scroll_top);
    }

    public void clear(RobotAction robot, int mainCityArmyNumber, int curIndex, int markIndex, int clearTabIndex) {
        //robot.leftMouseClick(getMrkByIndex(markIndex), WaitingTime.SELECT_MARK);
        robot.leftMouseClickMark(getMrkByIndex(markIndex),btn4);
        robot.leftMouseClickEx(getTabByIndex(clearTabIndex), WaitingTime.SELECT_TAB);
        robot.leftMouseClickEx(getArmyLocationInCity(curIndex<mainCityArmyNumber?curIndex:curIndex-mainCityArmyNumber), WaitingTime.SELECT_TROOP_IN_CITY);
        robot.leftMouseClickEx(confirm, WaitingTime.CONFIRM);
        robot.leftMouseClickEx(dangerous_confirm, WaitingTime.CONFIRM);
    }
    Coordinate getArmyLocationInCity(int index){
        return switch (index) {
            case 0 -> army1_from5;
            case 1 -> army2_from5;
            case 2 -> army3_from5;
            case 3 -> army4_from5;
            case 4 -> army5_from5;
            default -> null;
        };
    }
    Coordinate getMrkByIndex(int index){
        return switch (index) {
            case 1 -> mark1;
            case 2 -> mark2;
            case 3 -> mark3;
            case 4 -> mark4;
            default -> null;
        };
    }
    Coordinate getTabByIndex(int index){
        return switch (index) {
            case 1 -> tab1;
            case 2 -> tab2;
            case 3 -> tab3;
            default -> null;
        };
    }
}
