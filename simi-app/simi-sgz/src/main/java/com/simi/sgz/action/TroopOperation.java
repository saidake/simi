package com.simi.sgz.action;

import com.simi.common.util.data.RandomUtil;
import com.simi.sgz.AAAconfig.SgzConstants;
import com.simi.sgz.AAAconfig.WaitingTime;
import com.simi.sgz.domain.properties.Coordinate;
import com.simi.sgz.domain.properties.SimiSgz;
import lombok.AllArgsConstructor;
import lombok.Data;


@Data
@AllArgsConstructor
public class TroopOperation {
    private RobotAction robot;
    private SimiSgz simiSgz;
    private Coordinate coordinate;
    public void enterCity(int troopIndex){
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
    public void exitCity() {
        robot.leftMouseClickEx(coordinate.getCityBack());
    }
    public void sendMessenger(){
        robot.leftMouseClickEx(coordinate.getCityMessenger());
        robot.leftMouseClickEx(RandomUtil.getRandomElement(coordinate.getGiftLocation()));
        robot.leftMouseClickEx(coordinate.getSelectGift()[0]);
        robot.leftMouseClickEx(coordinate.getPurchaseGift());
        robot.leftMouseClickEx(coordinate.getSelectGift()[1]);
        robot.leftMouseClickEx(coordinate.getPurchaseGift());
        robot.leftMouseClickEx(coordinate.getSendMessenger());
        robot.leftMouseClickEx(coordinate.getCityBack());
        robot.leftMouseClickEx(coordinate.getCityBack());
    }
    public void visit() {
        robot.leftMouseClickEx(coordinate.getCityVisit());
        for (int i = 0; i < SgzConstants.VISIT_TIMES; i++) {
            robot.leftMouseClickEx(coordinate.getVisitBtn());
            robot.leftMouseClickEx(coordinate.getCityBack());
            robot.leftMouseClickEx(coordinate.getTroop3From5());
        }
        robot.leftMouseClickEx(coordinate.getCityBack());
    }
    public void trials() {
        robot.leftMouseClickEx(coordinate.getDetailBtn());
        robot.leftMouseClickEx(coordinate.getTrailBtn());
        robot.leftMouseClickEx(coordinate.getTrailStartBtn());
        robot.leftMouseClickEx(coordinate.getPromptConfirm());
        for (int i = 0; i < SgzConstants.TRIAL_TIMES; i++) {
            robot.leftMouseClickEx(coordinate.getTrailStartBtn());
        }
        robot.leftMouseClickEx(coordinate.getCityBack());
    }
    public void supplyArmy(int mainCityArmyNumber, int armyIndex){
        robot.leftMouseClickEx(getArmyLocationInCity(armyIndex<mainCityArmyNumber?armyIndex:armyIndex-mainCityArmyNumber));
        robot.leftMouseClickEx(coordinate.getCityAddTroops());
        robot.leftMouseClickEx(coordinate.getCityConfirm());
        robot.leftMouseClickEx(coordinate.getCityBack());
    }
    public void scrollToBottom(){
        int[] scrollTop = coordinate.getScrollTop();
        int[] scrollBot = coordinate.getScrollBot();
        robot.scroll(scrollBot,scrollTop);
        robot.scroll(scrollBot,scrollTop);
        robot.scroll(scrollBot,scrollTop);
        robot.scroll(scrollBot,scrollTop);
        robot.scroll(scrollBot,scrollTop);
        robot.scroll(scrollBot,scrollTop);
    }

    public void clear(int mainCityArmyNumber, boolean avoidMarchCollision, int curIndex, int markIndex, int clearTabIndex) {
        //robot.leftMouseClick(getMrkByIndex(markIndex), WaitingTime.SELECT_MARK);
        robot.leftMouseClickMark(getMrkByIndex(markIndex),coordinate.getBtn4(),avoidMarchCollision);
        robot.leftMouseClickEx(getTabByIndex(clearTabIndex), WaitingTime.SELECT_TAB);
        robot.leftMouseClickEx(getArmyLocationInCity(curIndex<mainCityArmyNumber?curIndex:curIndex-mainCityArmyNumber), WaitingTime.SELECT_TROOP_IN_CITY);
        robot.leftMouseClickEx(coordinate.getMarchConfirm(), WaitingTime.CONFIRM);
        robot.leftMouseClickEx(coordinate.getPromptConfirm(), WaitingTime.CONFIRM);
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
