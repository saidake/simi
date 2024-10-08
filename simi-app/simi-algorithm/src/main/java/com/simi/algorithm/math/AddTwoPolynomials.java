package com.simi.algorithm.math;

import com.simi.common.test.domain.PolyNode;
// https://leetcode.cn/problems/add-two-polynomials-represented-as-linked-lists/description/
public class AddTwoPolynomials {
    //Previous Solution
    public PolyNode addPoly(PolyNode poly1, PolyNode poly2) {
        //[IM] Assuming both polynomials are not empty first.
        //A. check for null value
        if(poly1==null&&poly2==null)return null;
        PolyNode head=null;
        PolyNode prev=null;
        PolyNode cur=poly1;
        while (poly1!=null||poly2!=null){
            //A. Ignore the polynomial with null value.
            if (poly2==null) {
                cur=new PolyNode(poly1.coefficient, poly1.power);
                poly1=poly1.next;
            } else if (poly1==null) {
                cur=new PolyNode(poly2.coefficient, poly2.power);
                poly2=poly2.next;
            //A. Merge coefficients when both polynomials have the same power.
            //[IM] Create a new PolyNode instead of operating the original node (Beneficial for handling node order).
            } else if(poly1.power==poly2.power){
                int mergeCoefficient = poly1.coefficient + poly2.coefficient;
                if(mergeCoefficient==0)cur=null;
                else cur=new PolyNode(poly1.coefficient+poly2.coefficient, poly1.power);
                poly1=poly1.next;
                poly2=poly2.next;
            }else if (poly1.power>poly2.power){
                cur=new PolyNode(poly1.coefficient, poly1.power);
                poly1=poly1.next;
            }else{
                cur=new PolyNode(poly2.coefficient, poly2.power);
                poly2=poly2.next;
            }
            //A. initialize head node.
            if(prev==null)head=cur;
            //A. Move cursors.
            else if(cur!=null)prev.next=cur;
            if(cur!=null) prev=cur;

        }
        return head;
    }
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


