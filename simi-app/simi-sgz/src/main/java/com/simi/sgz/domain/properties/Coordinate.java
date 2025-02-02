package com.simi.sgz.domain.properties;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class Coordinate implements Serializable {
    private String name;
    private int[] redPoint;
    private int[] stopBtn1;
    private int[] detailBtn;
    private int[] trailBtn;
    private int[] trailStartBtn;
    private int[] cityVisit;
    private int[] visitBtn;
    private int[] cityMessenger;
    private int[][] giftLocation;
    private int[][] selectGift;
    private int[] purchaseGift;
    private int[] sendMessenger;
    private int[] marchConfirm;
    private int[] promptConfirm;
    private int[] cityAddTroops;
    private int[] cityConfirm;
    private int[] cityBack;
    private int[] armyBtn;
    private int[] coordinateBtn;
    private int[] scrollTop;
    private int[] scrollBot;
    private int[] btn1;
    private int[] btn2;
    private int[] btn3;
    private int[] btn4;
    private int[] tab1;
    private int[] tab2;
    private int[] tab3;
    private int[] troop1From1;
    private int[] troop1From5;
    private int[] troop2From5;
    private int[] troop3From5;
    private int[] troop4From5;
    private int[] troop5From5;
    private int[] mark1;
    private int[] mark2;
    private int[] mark3;
    private int[] mark4;
}
