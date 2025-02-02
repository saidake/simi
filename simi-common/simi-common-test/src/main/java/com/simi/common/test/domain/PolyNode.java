package com.simi.common.test.domain;

public class PolyNode {
    public int coefficient, power;
    public PolyNode next;

    public PolyNode() {
    }
    public PolyNode(int coefficient, int power) {
        this.coefficient = coefficient;
        this.power = power;
    }

    public PolyNode(int coefficient, int power, PolyNode next) {
        this.coefficient = coefficient;
        this.power = power;
        this.next = next;
    }

}
