package com.simi.algorithm.math;

import com.simi.common.test.domain.PolyNode;
// https://leetcode.cn/problems/add-two-polynomials-represented-as-linked-lists/description/
/*
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
        //A. check for null value;
        if(poly1==null&&poly2==null)return null;
        //A. traverse the cur node.
        /*
            10      9    5    0
            poly1
             8      5    1    0
            poly2
         */
        PolyNode head=null;
        PolyNode prev=null;
        PolyNode cur=poly1;
        while (poly1.next!=null||poly2.next!=null){
            //A. Assuming they all have values first.
            //A. Merge coefficient when both polynomial have the same power.
            if(poly1.power==poly2.power){
                cur=new PolyNode(poly1.coefficient+poly2.coefficient, poly1.power);
            }else if (poly1.power>poly2.power){
                cur=new PolyNode(poly1.coefficient, poly1.power);
            }else{
                cur=new PolyNode(poly2.coefficient, poly2.power);
            }
            if(prev!=null){
                prev.next=cur;
                prev=cur;
            }else{
                head=cur;
            }
            poly1=poly1.next;
            poly2=poly2.next;
        }
        return head;
    }
}
