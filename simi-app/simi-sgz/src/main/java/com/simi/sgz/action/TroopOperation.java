package com.simi.sgz.action;

import com.simi.sgz.AAAconfig.WaitingTime;
import com.simi.sgz.domain.properties.Coordinate;
import com.simi.sgz.domain.properties.SimiSgz;
import lombok.AllArgsConstructor;
import lombok.Data;


@Data
@AllArgsConstructor
public class TroopOperation {
    private SimiSgz simiSgz;
    private Coordinate coordinate;
    public void enterCity(RobotAction robot, int troopIndex){
        robot.leftMouseClickEx(coordinate.getCoordinateBtn());
        int[] scrollTop = coordinate.getScrollTop();
        int[] scrollBot = coordinate.getScrollBot();
        robot.scroll(scrollTop,scrollBot);
        robot.scroll(scrollTop,scrollBot);
        robot.scroll(scrollTop,scrollBot);
        robot.scroll(scrollTop,scrollBot);
        robot.scroll(scrollTop,scrollBot);
        robot.scroll(scrollTop,scrollBot);
        robot.leftMouseClickEx(troopIndex< simiSgz.getMainCityArmyNumber()?coordinate.getMark4():coordinate.getMark3(), WaitingTime.SELECT_MARK);
        robot.leftMouseClickEx(coordinate.getRedPoint(), WaitingTime.ENTER_CITY);
    }


    public void supplyArmy(RobotAction robot, int mainCityArmyNumber, int armyIndex){
        robot.leftMouseClickEx(getArmyLocationInCity(armyIndex<mainCityArmyNumber?armyIndex:armyIndex-mainCityArmyNumber));
        robot.leftMouseClickEx(coordinate.getCityAddTroops());
        robot.leftMouseClickEx(coordinate.getCityConfirm());
        robot.leftMouseClickEx(coordinate.getCityBack());
    }
    public void goBackAndScrollToBottom(RobotAction robot){
        robot.leftMouseClickEx(coordinate.getCityBack());
        int[] scrollTop = coordinate.getScrollTop();
        int[] scrollBot = coordinate.getScrollBot();
        robot.scroll(scrollBot,scrollTop);
        robot.scroll(scrollBot,scrollTop);
        robot.scroll(scrollBot,scrollTop);
        robot.scroll(scrollBot,scrollTop);
        robot.scroll(scrollBot,scrollTop);
        robot.scroll(scrollBot,scrollTop);
    }

    public void clear(RobotAction robot, int mainCityArmyNumber, int curIndex, int markIndex, int clearTabIndex) {
        //robot.leftMouseClick(getMrkByIndex(markIndex), WaitingTime.SELECT_MARK);
        robot.leftMouseClickMark(getMrkByIndex(markIndex),coordinate.getBtn4());
        robot.leftMouseClickEx(getTabByIndex(clearTabIndex), WaitingTime.SELECT_TAB);
        robot.leftMouseClickEx(getArmyLocationInCity(curIndex<mainCityArmyNumber?curIndex:curIndex-mainCityArmyNumber), WaitingTime.SELECT_TROOP_IN_CITY);
        robot.leftMouseClickEx(coordinate.getConfirm(), WaitingTime.CONFIRM);
        robot.leftMouseClickEx(coordinate.getDangerousConfirm(), WaitingTime.CONFIRM);
    }
    int[] getArmyLocationInCity(int index){
        return switch (index) {
            case 0 -> coordinate.getTroop1From5();
            case 1 -> coordinate.getTroop2From5();
            case 2 -> coordinate.getTroop3From5();
            case 3 -> coordinate.getTroop4From5();
            case 4 -> coordinate.getTroop5From5();
            default -> null;
        };
    }
    int[] getMrkByIndex(int index){
        return switch (index) {
            case 1 -> coordinate.getMark1();
            case 2 -> coordinate.getMark2();
            case 3 -> coordinate.getMark3();
            case 4 -> coordinate.getMark4();
            default -> null;
        };
    }
    int[] getTabByIndex(int index){
        return switch (index) {
            case 1 -> coordinate.getTab1();
            case 2 -> coordinate.getTab2();
            case 3 -> coordinate.getTab3();
            default -> null;
        };
    }
}
