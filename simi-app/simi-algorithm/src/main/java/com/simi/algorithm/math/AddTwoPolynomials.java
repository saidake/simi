package com.simi.algorithm.math;

import com.simi.common.test.domain.PolyNode;

/***********************************************************
--------------
 10  9   5   0
 cur
  8  5   1   0
 cmp
-------------- skip
 10  9   5   0
    cur
  8  5   1   0
 cmp
-------------- insert cmp
 10  9  8  5   0
       cur
  5   1   0
 cmp
-------------- skip
 10  9  8  5   0
          cur
  5   1   0
 cmp
-------------- delete the two nodes with coefficients of 0 after calculation.
 10  9  8  5   0
          cur
  5   1   0
 cmp
 ***********************************************************/
public class AddTwoPolynomials {
    //Previous Solution
    public PolyNode addPoly(PolyNode poly1, PolyNode poly2) {
        //A. Pay attention to the conditional variable.
        //A. check for null value;
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
