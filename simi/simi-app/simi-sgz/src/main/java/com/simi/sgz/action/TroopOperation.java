package com.simi.sgz.action;

import com.simi.common.util.data.RandomUtil;
import com.simi.sgz.AAAconfig.SgzConstants;
import com.simi.sgz.AAAconfig.WaitingTime;
import com.simi.sgz.domain.properties.Coordinate;
import com.simi.sgz.domain.properties.SimiSgz;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import net.sourceforge.tess4j.Tesseract;
import net.sourceforge.tess4j.TesseractException;

import java.awt.*;
import java.awt.image.BufferedImage;

@Data
@AllArgsConstructor
@Slf4j
public class TroopOperation {
    private RobotAction robot;
    private SimiSgz simiSgz;
    private Coordinate coordinate;

    public void enterCity(int troopIndex){
        robot.leftMouseClickEx(coordinate.getCoordinateBtn());
        this.scrollToTop();
        robot.leftMouseClickEx(troopIndex< SgzConstants.CITY_TROOP_NUMBER ?coordinate.getMark4():coordinate.getMark3(), WaitingTime.SELECT_MARK);
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
        robot.leftMouseClickEx(getMrkByIndex(1));
    }
    public void trials() {
        robot.leftMouseClickEx(coordinate.getDetailBtn());
        robot.leftMouseClickEx(coordinate.getTrailBtn());
        robot.leftMouseClickEx(coordinate.getTrailStartBtn());
        robot.leftMouseClickEx(coordinate.getPromptConfirm());
        for (int i = 0; i < SgzConstants.TRIAL_TIMES; i++) {
            robot.leftMouseClickEx(coordinate.getTrailStartBtn());
        }
    }
    public void replenishTroop(int troopIndex){
        robot.leftMouseClickEx(getTroopLocationInCity(troopIndex%SgzConstants.CITY_TROOP_NUMBER ));
        robot.leftMouseClickEx(coordinate.getCityAddTroops());
        robot.leftMouseClickEx(coordinate.getCityConfirm());
        robot.leftMouseClickEx(coordinate.getCityBack());
    }

    public void initializeStamina(int troopIndex, int[] staminaList) throws TesseractException {
        robot.leftMouseClickEx(getTroopLocationInCity(troopIndex%SgzConstants.CITY_TROOP_NUMBER ));
        robot.leftMouseClickEx(coordinate.getRedPoint());
        int x1=coordinate.getStaminaOcrUpperTop()[0];
        int y1=coordinate.getStaminaOcrUpperTop()[1];
        int x2=coordinate.getStaminaOcrBottomRight()[0];
        int y2=coordinate.getStaminaOcrBottomRight()[1];
        Rectangle captureRect = new Rectangle(x1, y1, (x2 - x1)*3, (y2 - y1)*3);
        BufferedImage screenCapture = robot.createScreenCapture(captureRect);
        // Initialize Tesseract
        Tesseract tesseract = new Tesseract();
        tesseract.setDatapath(SgzConstants.TESSERACT_ENGIN_PATH); // Path to Tesseract's tessdata directory
        // Perform OCR
        String result=tesseract.doOCR(screenCapture);
        log.info("Detected number: {}", result.trim());
        String processedStr=result.trim()
                .replaceAll("B","8")
                .replaceAll("A","4")
                .replaceAll("O","0")
                .replaceAll("¢","0")
                .replaceAll("E","6")
                .replaceAll("\\D+","");
        int stamina=processedStr.isBlank()?0:Integer.parseInt(processedStr);
        log.info("Processed number: {}", stamina);
        if(staminaList[troopIndex]==1){
            staminaList[troopIndex]=stamina>=15?stamina:0;
        } else if(staminaList[troopIndex]>1){
            staminaList[troopIndex]= stamina>=15?Math.max(stamina - staminaList[troopIndex], 0):0;
        }
        robot.leftMouseClickEx(coordinate.getCloseWindow());
        robot.leftMouseClickEx(coordinate.getCityBack());
//        if(levelUpProperties.getMinus()!=0){
//            for (int i = 0; i < staminaList.length; i++) {
//                for (int j = 0; j < staminaList[0].length; j++) {
//                    staminaList[i][j]-=levelUpProperties.getMinus();
//                }
//            }
//        }
    }

    public void scrollToBottom(){
        this.scroll(coordinate.getScrollBot(), coordinate.getScrollTop());
    }
    public void scrollToTop(){
        this.scroll(coordinate.getScrollTop(), coordinate.getScrollBot());
    }
    public void dispatchTroop(){
        robot.leftMouseClickEx(coordinate.getRedPoint());
        robot.leftMouseClickEx(coordinate.getBtn2());
        robot.leftMouseClickEx(coordinate.getTab1());
        robot.leftMouseClickEx(coordinate.getTroop1From5());
        robot.leftMouseClickEx(coordinate.getMarchConfirm());
        robot.leftMouseClickEx(coordinate.getRedPoint());
        robot.leftMouseClickEx(coordinate.getBtn2());
        robot.leftMouseClickEx(coordinate.getTab2());
        robot.leftMouseClickEx(coordinate.getTroop1From5());
        robot.leftMouseClickEx(coordinate.getMarchConfirm());
    }
    public void scroll(int[] scrollFrom, int[] scrollTo){
        robot.scroll(scrollFrom,scrollTo);
        robot.scroll(scrollFrom,scrollTo);
        robot.scroll(scrollFrom,scrollTo);
        robot.scroll(scrollFrom,scrollTo);
        robot.scroll(scrollFrom,scrollTo);
        robot.scroll(scrollFrom,scrollTo);
    }
    public void clear(boolean avoidMarchCollision, int curIndex, int markIndex, int clearTabIndex) {
        robot.leftMouseClickMark(getMrkByIndex(markIndex),coordinate.getBtn4(),avoidMarchCollision);
        // Select a tab
        robot.leftMouseClickEx(getTabByIndex(clearTabIndex), WaitingTime.SELECT_TAB);
        robot.leftMouseClickEx(getArmySelectionLocationInCity(curIndex), WaitingTime.SELECT_TROOP_IN_CITY);
        robot.leftMouseClickEx(coordinate.getMarchConfirm(), WaitingTime.CONFIRM);
        robot.leftMouseClickEx(coordinate.getPromptConfirm(), WaitingTime.CONFIRM);
    }
    int[] getTroopLocationInCity(int index){
        return switch (index) {
            case 0 -> coordinate.getTroop1From5();
            case 1 -> coordinate.getTroop2From5();
            case 2 -> coordinate.getTroop3From5();
            case 3 -> coordinate.getTroop4From5();
            case 4 -> coordinate.getTroop5From5();
            default -> null;
        };
    }

    int[] getArmySelectionLocationInCity(int index){
        return switch (index) {
            case 15 -> coordinate.getTroop1From5();
            case 25 -> coordinate.getTroop2From5();
            case 35 -> coordinate.getTroop3From5();
            case 45 -> coordinate.getTroop4From5();
            case 55 -> coordinate.getTroop5From5();

            case 14 -> coordinate.getTroop1From4();
            case 24 -> coordinate.getTroop2From4();
            case 34 -> coordinate.getTroop3From4();
            case 44 -> coordinate.getTroop4From4();
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
