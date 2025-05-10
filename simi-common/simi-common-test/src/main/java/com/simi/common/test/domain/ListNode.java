package com.simi.common.test.domain;

import lombok.Data;

@Data
public class ListNode {
    public int val;
    public ListNode next;
    public ListNode(int x) {
        this.val = x;
    }
}
