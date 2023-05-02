package com.saidake.trade.domain.qihuo;

import com.alibaba.excel.context.AnalysisContext;
import com.alibaba.excel.read.listener.ReadListener;
import com.google.common.base.Enums;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;

@Slf4j
@Data
public class AccountInfoExcelListener implements ReadListener<AccountInfoExcel> {

    QihuoAccountInfoResponse qihuoAccountInfoResponse=new QihuoAccountInfoResponse();
    Map<String, VarietyData> generalVarietyDataMap=new HashMap<>();
    Map<String, VarietyData> realVarietyDataMap=new HashMap<>();
    Map<String, VarietyData> simulatedVarietyDataMap=new HashMap<>();

    /**
     * invoke
     *
     * @param accountInfoExcel
     * @param analysisContext
     */
    @Override
    public void invoke(AccountInfoExcel accountInfoExcel, AnalysisContext analysisContext) {
        //A. 筛选条件
        if(StringUtils.isBlank(accountInfoExcel.getNetProfit()))return;
        String cost = Enums.getIfPresent(TradeCostEnum.class, accountInfoExcel.getCode()).transform(item -> item.getCost().toString()).orNull();
//        if(cost==null)return;
        if(cost==null)cost="0";
        BigDecimal netProfit = new BigDecimal(accountInfoExcel.getNetProfit());
        BaseAccountInfo baseAccountInfo;
        Map<String, VarietyData> varietyDataMap;
        if(AccountType.REAL.toString().equals(accountInfoExcel.getAccountType())){
            baseAccountInfo=qihuoAccountInfoResponse.getReal();
            varietyDataMap=realVarietyDataMap;
        }else if(AccountType.SIMULATED.toString().equals(accountInfoExcel.getAccountType())){
            baseAccountInfo=qihuoAccountInfoResponse.getSimulated();
            varietyDataMap=simulatedVarietyDataMap;
        }else {
            return;
        }
        //A. 宏观属性设置
        BigDecimal currentNetProfit =new BigDecimal(StringUtils.defaultIfBlank(accountInfoExcel.getNetProfit(),"0"));
        BigDecimal currentCost =new BigDecimal("-"+cost).multiply(new BigDecimal(accountInfoExcel.getNum()));
        //A. general 信息
        setAccountInfo(accountInfoExcel, netProfit, qihuoAccountInfoResponse.getGeneral(), generalVarietyDataMap, currentNetProfit, currentCost);
        //A. real 或 simulated 信息
        setAccountInfo(accountInfoExcel, netProfit, baseAccountInfo, varietyDataMap, currentNetProfit, currentCost);
    }

    /**
     * setAccountInfo
     *
     * @param accountInfoExcel
     * @param netProfit
     * @param baseAccountInfo
     * @param varietyDataMap
     * @param currentNetProfit
     * @param currentCost
     */
    private void setAccountInfo(AccountInfoExcel accountInfoExcel, BigDecimal netProfit, BaseAccountInfo baseAccountInfo,
                                Map<String, VarietyData> varietyDataMap,  BigDecimal currentNetProfit, BigDecimal currentCost) {
        setBaseInfo(baseAccountInfo,netProfit,currentNetProfit,currentCost);
        //A. 品种属性设置
        if(!varietyDataMap.containsKey(accountInfoExcel.getName())){
            varietyDataMap.put(accountInfoExcel.getName(),new VarietyData(){{
                setName(accountInfoExcel.getName());
                setCode(accountInfoExcel.getCode());
            }});
        }
        VarietyData varietyData = varietyDataMap.get(accountInfoExcel.getName());
        setBaseInfo(varietyData,netProfit,currentNetProfit,currentCost);
        Map<String, StrategyData> strategyDataMap = varietyData.getStrategyDataMap();
        //A. 策略属性设置
        if(!strategyDataMap.containsKey(accountInfoExcel.getTradeReason())){
            strategyDataMap.put(accountInfoExcel.getTradeReason(),new StrategyData(){{
                setName(accountInfoExcel.getTradeReason());
                setRule(accountInfoExcel.getRule());
            }});
        }
        StrategyData strategyData = strategyDataMap.get(accountInfoExcel.getTradeReason());
        setBaseInfo(strategyData,netProfit,currentNetProfit,currentCost);
    }

    /**
     * doAfterAllAnalysed
     *
     * @param analysisContext
     */
    @Override
    public void doAfterAllAnalysed(AnalysisContext analysisContext) {
        log.info("after all qihuoAccountInfoResponse: {}",qihuoAccountInfoResponse);
        setAfterAllBaseInfo(qihuoAccountInfoResponse.getGeneral());
        setAfterAllBaseInfo(qihuoAccountInfoResponse.getReal());
        setAfterAllBaseInfo(qihuoAccountInfoResponse.getSimulated());
        handleAfterAllVarietyDataMap(generalVarietyDataMap);
        handleAfterAllVarietyDataMap(realVarietyDataMap);
        handleAfterAllVarietyDataMap(simulatedVarietyDataMap);

        qihuoAccountInfoResponse.getGeneral().setVarietyDataMap(generalVarietyDataMap);
        qihuoAccountInfoResponse.getReal().setVarietyDataMap(realVarietyDataMap);
        qihuoAccountInfoResponse.getSimulated().setVarietyDataMap(simulatedVarietyDataMap);

        qihuoAccountInfoResponse.getGeneral().setBaseAssets(GlobalAccountInfo.REAL_ASSETS+GlobalAccountInfo.SIMULATED_ASSETS);
        qihuoAccountInfoResponse.getGeneral().setWinPercent(qihuoAccountInfoResponse.getGeneral().getTotalNetProfit().multiply(new BigDecimal(100))
                .divide(new BigDecimal(GlobalAccountInfo.REAL_ASSETS+GlobalAccountInfo.SIMULATED_ASSETS),2));
        qihuoAccountInfoResponse.getSimulated().setBaseAssets(GlobalAccountInfo.SIMULATED_ASSETS);
        qihuoAccountInfoResponse.getSimulated().setWinPercent(qihuoAccountInfoResponse.getSimulated().getTotalNetProfit().multiply(new BigDecimal(100))
                .divide(new BigDecimal(GlobalAccountInfo.SIMULATED_ASSETS),2));
        qihuoAccountInfoResponse.getReal().setBaseAssets(GlobalAccountInfo.REAL_ASSETS);
        qihuoAccountInfoResponse.getReal().setWinPercent(qihuoAccountInfoResponse.getReal().getTotalNetProfit().multiply(new BigDecimal(100))
                .divide(new BigDecimal(GlobalAccountInfo.REAL_ASSETS),2));
    }

    /**
     * handleAfterAllVarietyDataMap
     *
     * @param totalInfoMap
     * @param <T>
     * @return
     */
    private <T extends TotalInfo> void handleAfterAllVarietyDataMap(Map<String, T> totalInfoMap) {
        totalInfoMap.entrySet().parallelStream().forEach(entry -> {
            T value = entry.getValue();
            setAfterAllBaseInfo(value);
        });
    }

    /**
     * setAfterAllBaseInfo
     *
     * @param totalInfo
     */
    private void setAfterAllBaseInfo(TotalInfo totalInfo) {
        totalInfo.setTotalWiningRate(
                new BigDecimal(totalInfo.getTotalProfitTimes()*100).divide(new BigDecimal(totalInfo.getTotalProfitTimes() + totalInfo.getTotalLossTimes()),2,BigDecimal.ROUND_HALF_UP)
        );
    }

    //================================================================== 工具类

    /**
     * 设置账号，品种，策略信息（交易次数，盈利次数，亏损次数）
     *
     * @param totalInfo
     * @param netProfit
     * @param currentNetProfit
     * @param currentCost
     */
    private void setBaseInfo(TotalInfo totalInfo, BigDecimal netProfit,BigDecimal currentNetProfit, BigDecimal currentCost) {
        BigDecimal bigDecimal = new BigDecimal(0);
        totalInfo.setTotalTradeTimes(totalInfo.getTotalTradeTimes()+1);
        totalInfo.setTotalNetProfit(totalInfo.getTotalNetProfit().add(currentNetProfit).add(currentCost));
        if(netProfit.compareTo(bigDecimal) > 0){
            totalInfo.setTotalProfitTimes(totalInfo.getTotalProfitTimes()+1);
        }else if(netProfit.compareTo(bigDecimal) < 0){
            totalInfo.setTotalLossTimes(totalInfo.getTotalLossTimes()+1);
        }
    }

}
