package com.simi.common.test.pojo;

import lombok.Data;
import lombok.Getter;

@Data
public class ListNode {
    public int val;
    public ListNode next;
    public ListNode(int x) {
        this.val = x;
    }
}
