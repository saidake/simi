package com.saidake.trade.domain.qihuo;

import lombok.Data;

@Data
public class QihuoAccountInfoResponse  {
    private BaseAccountInfo general=new BaseAccountInfo();
    private BaseAccountInfo real=new BaseAccountInfo();
    private BaseAccountInfo simulated=new BaseAccountInfo();
}
