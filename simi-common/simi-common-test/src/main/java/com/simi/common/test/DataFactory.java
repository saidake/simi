package com.simi.common.test;

import com.simi.common.test.pojo.ListNode;
import com.simi.common.test.pojo.Person;
import com.simi.common.test.pojo.Person.Gender;
import com.simi.common.util.data.RandomUtil;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.List;

@UtilityClass
@Slf4j
public class DataFactory {
    private int DEFAULT_LENGTH=10;
    private int DEFAULT_LINKED_LIST_LENGTH=10;
    private int DEFAULT_OBJECT_LIST_LENGTH=10;
    private int DEFAULT_LOWER=0;
    private int DEFAULT_UPPER=9999;

    public static <T> void printArr(T ...ele){
        for (int i = 0; i < ele.length; i++) {
            log.info("ele [{}]: {}",i,ele[i]);
        }
    }
    public static <T> void printListNode(ListNode listNode){
        while (listNode!=null){
            log.info("list node: {}", listNode.val);
            listNode=listNode.next;
        }
    }
    public static List<Integer> getIntList(){
        return getIntList(DEFAULT_LENGTH, DEFAULT_LOWER,DEFAULT_UPPER);
    }
    public static List<Person> getPersonList(){
        List<Person> personList= new ArrayList<>();
        for (int i = 0; i < DEFAULT_OBJECT_LIST_LENGTH; i++) {
            int randomInt = RandomUtil.getRandomInt(DEFAULT_LOWER, DEFAULT_UPPER);
            personList.add(
                    new Person(
                            RandomUtil.getRandomEnglishName(),
                            RandomUtil.getRandomAge(),
                            RandomUtil.getRandomElement(Gender.MALE,Gender.FEMALE)
                            ));
        }
        return personList;
    }
    public static ListNode getLinkedList(){
        ListNode head = new ListNode(RandomUtil.getRandomInt(DEFAULT_LOWER,DEFAULT_UPPER));
        ListNode result=head;
        for (int i = 0; i < DEFAULT_LINKED_LIST_LENGTH; i++) {
            head.next=new ListNode(RandomUtil.getRandomInt(DEFAULT_LOWER,DEFAULT_UPPER));
            head=head.next;
        }
        return result;
    }

    public static List<Integer> getIntList(int length, int lower, int upper){
        ArrayList<Integer> intList = new ArrayList<>();
        for (int i = 0; i < length; i++) {
            intList.add(RandomUtil.getRandomInt(lower,upper));
        }
        return intList;
    }
    public static int[] getPrimitiveIntArray(int length, int lower, int upper){
        int[] intArr=new int[length];
        for (int i = 0; i < length; i++) {
            intArr[i]=RandomUtil.getRandomInt(lower,upper);
        }
        return intArr;
    }
    public static int[] getPrimitiveIntArray(){
        return getPrimitiveIntArray(DEFAULT_LENGTH, DEFAULT_LOWER,DEFAULT_UPPER);
    }

    public static String getRandomString(){
        return RandomUtil.getRandomString();
    }
}
