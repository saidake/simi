package com.simi;

import com.simi.common.test.DataFactory;
import com.simi.common.test.domain.ListNode;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class Solution {
    public static ListNode reverseList(ListNode head) {
        // null = cur.next
        // prev.next = cur
        ListNode prev=head;
        ListNode next=head.next;
        while (next!=null){
            ListNode temp=next.next;
            next.next=prev;
            prev=next;
            next=temp;
        }
        return prev;
    }
    public static void main(String[] args) {
        ListNode linkedList = DataFactory.getLinkedList();
        DataFactory.printListNode(linkedList);
        reverseList(linkedList);
        System.out.println("===========================");
        DataFactory.printListNode(linkedList);
    }
}
