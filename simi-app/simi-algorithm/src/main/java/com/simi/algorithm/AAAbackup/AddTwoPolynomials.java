package com.simi.algorithm.AAAbackup;

import com.simi.common.test.domain.PolyNode;

// https://leetcode.cn/problems/add-two-polynomials-represented-as-linked-lists/description/
public class AddTwoPolynomials {
    public PolyNode addPolyPrevious(PolyNode poly1, PolyNode poly2) {
        if(poly1==null&&poly2==null)return null;
        //A. traverse the cur node.
        PolyNode cur=poly1.power>poly2.power?poly1:poly2;
        PolyNode cmp=poly1.power>poly2.power?poly2:poly1;
        PolyNode head=cur;
        PolyNode prev=null;
        while(cur!=null&&cmp!=null) {
            if(cur.power==cmp.power){
                //B. merge coefficients.
                int mCo= cur.coefficient+cmp.coefficient;
                cur.coefficient=mCo;
                //B. delete the node with 0 coefficient.
                if(mCo==0){
                    if(prev==null){
                        if(cur.next!=null&&cmp.next!=null){
                            cur=cur.next.power>cmp.next.power?cur.next:cmp.next;
                            cmp=cur.next.power>cmp.next.power?cmp.next:cur.next;
                            head=cur;
                        } else {
                            if(head==cur)head=cur.next;
                            cur=cur.next;
                            cmp=cmp.next;
                        }
                    }else{
                        //C. move pointer.
                        prev.next=cur.next;
                        cur=cur.next;
                        cmp=cmp.next;
                    }
                }else{
                    prev=cur;
                    //B. move pointer.
                    cur=cur.next;
                    cmp=cmp.next;
                }
            }else if(cur.power>cmp.power&&cur.next!=null&&cur.next.power<cmp.power ||cur.next==null){
                //B. insert the cmp between cur and next.
                //C. insert cmp.
                PolyNode temp=cur.next;
                cur.next=cmp;
                PolyNode cmpTemp=cmp.next;
                cmp.next=temp;
                //C. move pointer.
                prev=cur;
                cur=cmp;
                cmp=cmpTemp;
            }else{
                prev=cur;
                cur=cur.next;
            }

        }
        return head;
    }
}

