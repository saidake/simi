package com.saidake.trade.domain.fuhui;

import lombok.Data;

import java.util.LinkedList;

@Data
public class AccountResponse {
    LinkedList<Account> accountLinkedList;
    String totalTime;
}
