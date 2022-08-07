package com.saidake.citi.domain.account;

import lombok.Data;

import java.util.LinkedList;

@Data
public class AccountResponse {
    LinkedList<Account> accountLinkedList;
    String totalTime;
}
