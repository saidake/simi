package com.simi.service.trade.domain;

import lombok.Data;

import java.util.LinkedList;

@Data
public class AccountResponse {
    LinkedList<Account> accountLinkedList;
    String totalTime;
}
