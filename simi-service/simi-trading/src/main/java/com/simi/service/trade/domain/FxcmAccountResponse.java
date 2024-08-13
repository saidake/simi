package com.simi.service.trade.domain;

import lombok.Data;

import java.util.LinkedList;
import java.util.List;

@Data
public class FxcmAccountResponse {
    List<FxcmAccount> fxcmAccountList = new LinkedList<>();
}
