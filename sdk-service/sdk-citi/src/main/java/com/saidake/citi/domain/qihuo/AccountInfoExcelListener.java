package com.saidake.citi.domain.qihuo;

import com.alibaba.excel.context.AnalysisContext;
import com.alibaba.excel.read.listener.ReadListener;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.text.DecimalFormat;
import java.util.Map;

@Slf4j
@Data
public class AccountInfoExcelListener  implements ReadListener<AccountInfoExcel> {

    QihuoAccountInfoResponse qihuoAccountInfoResponse=new QihuoAccountInfoResponse();

    @Override
    public void invoke(AccountInfoExcel accountInfoExcel, AnalysisContext analysisContext) {
        if(StringUtils.isBlank(accountInfoExcel.getNetProfit()))return;
        Double netProfit = Double.valueOf(accountInfoExcel.getNetProfit());
        //log.info("current qihuoAccountInfoResponse: {}",qihuoAccountInfoResponse);
        //A. 前置属性设置，不考虑 0 的情况
        setBaseInfo(qihuoAccountInfoResponse,netProfit);
        //A. 宏观属性设置
        qihuoAccountInfoResponse.setTotalNetProfit(qihuoAccountInfoResponse.getTotalNetProfit()+
                (accountInfoExcel.getNetProfit()!=null?Integer.valueOf(accountInfoExcel.getNetProfit()):0)
        );
        //A. 品种属性设置
        Map<String, VarietyData> varietyDataMap = qihuoAccountInfoResponse.getVarietyDataMap();
        if(!varietyDataMap.containsKey(accountInfoExcel.getCode())){
            varietyDataMap.put(accountInfoExcel.getCode(),new VarietyData(){{
                setName(accountInfoExcel.getName());
            }});
        }
        VarietyData varietyData = varietyDataMap.get(accountInfoExcel.getCode());
        setBaseInfo(varietyData,netProfit);
    }

    @Override
    public void doAfterAllAnalysed(AnalysisContext analysisContext) {
        log.info("after all qihuoAccountInfoResponse: {}",qihuoAccountInfoResponse);
        setAfterAllBaseInfo(qihuoAccountInfoResponse);
        qihuoAccountInfoResponse.getVarietyDataMap().values().parallelStream().forEach(this::setAfterAllBaseInfo);
    }

    private void setAfterAllBaseInfo(BaseInfo baseInfo) {
        baseInfo.setTotalWiningRate(
                new DecimalFormat("0.00").format(
                        ((double)baseInfo.getTotalProfitTimes() /
                                        (baseInfo.getTotalProfitTimes() + baseInfo.getTotalLossTimes())
                                 ) * 100
                )
        );
    }

    //================================================================== 工具类
    private void setBaseInfo(BaseInfo baseInfo,Double netProfit) {
        baseInfo.setTotalTradeTimes(baseInfo.getTotalTradeTimes()+1);
        if(netProfit>0){
            baseInfo.setTotalProfitTimes(baseInfo.getTotalProfitTimes()+1);
        }else if(netProfit<0){
            baseInfo.setTotalLossTimes(baseInfo.getTotalLossTimes()+1);
        }
    }

}
