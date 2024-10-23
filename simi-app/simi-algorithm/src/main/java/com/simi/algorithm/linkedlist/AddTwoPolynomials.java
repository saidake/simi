package com.simi.algorithm.linkedlist;

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
}


