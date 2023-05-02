import React, { useState, useEffect, Fragment } from "react";
import {
  accountData,
  IAccountResponseData,
  ITotalInfo,
  IStragetyMap,
  IVarietyMap,
  IBaseAccountInfo,
} from "@/store/atoms/DashboardAtom";
import {
  Chart,
  Line,
  Point,
  Tooltip,
  Axis,
  Interaction,
  Legend,
  Interval,
  Annotation,
  Coordinate,
  getTheme,
} from "bizcharts";
import { useRecoilValue } from "recoil";

export interface IToTalPage {
  menuIconIndex: number;
  menuVarietyType: string;
}

const ToTalPage: React.FC<IToTalPage> = React.memo(
  ({ menuIconIndex, menuVarietyType }) => {
    const recoilAccountData = useRecoilValue(accountData);

    const [currentAccountTotalInfo, setCurrentAccountTotalInfo] =
      useState<IBaseAccountInfo>(recoilAccountData.general);

    useEffect(() => {
      switch (menuIconIndex) {
        case 0:
          setCurrentAccountTotalInfo(recoilAccountData.general);
          break;
        case 1:
          setCurrentAccountTotalInfo(recoilAccountData.simulated);
          break;
        case 2:
          setCurrentAccountTotalInfo(recoilAccountData.real);
          break;
      }
    }, [menuIconIndex]);

    //———————————————————————————————————————————————————————————— 数据处理方法 ——————————————————————————————————————————————————————————————————————//
    const renderAccountType = () => {
      switch (menuIconIndex) {
        case 0:
          return "SIMULATED + REAL";
        case 1:
          return "SIMULATED";
        case 2:
          return "REAL";
      }
    };

    const getPinChartLabelPattern = (chartType: string, data: any) => {
      let labelSuffix;
      switch (chartType) {
        case "winrate":
          labelSuffix = "%";
          break;
        case "count":
          labelSuffix = "次";
          break;
      }
      return `${data.variety}: ${data.label} ${labelSuffix}`;
    };

    const getVarietyProfitOrLossTimesChartData = (
      dataType: string,
      chartType: string
    ) => {
      let varietyDataMap: { [xxx: string]: IVarietyMap } =
        currentAccountTotalInfo.varietyDataMap;
      let resultDataMap: { [xxx: string]: ITotalInfo };
      //======================================================== variety info
      let generalWiningRate = 0;
      if (dataType == "variety") {
        resultDataMap = varietyDataMap;
      } else if (dataType == "strategy" && !!menuVarietyType) {
        let stragetyDataMap: { [xxx: string]: IStragetyMap } =
          varietyDataMap[menuVarietyType].strategyDataMap;
          console.log("stragetyDataMap",stragetyDataMap)
        resultDataMap = stragetyDataMap;
      } else {
        resultDataMap = varietyDataMap;
      }

      for (let currentVarietyKey in resultDataMap) {
        generalWiningRate += parseFloat(
          resultDataMap[currentVarietyKey].totalWiningRate
        );
      }
      return Object.keys(resultDataMap).map((currentKey) => {
        let item = resultDataMap[currentKey];
        let resultLabel, resultPercent;
        switch (chartType) {
          case "winrate":
            resultLabel = parseFloat(item.totalWiningRate);
            resultPercent =
              parseFloat(item.totalWiningRate) / generalWiningRate;
            break;
          case "count":
            resultLabel = item.totalTradeTimes;
            resultPercent =
              item.totalTradeTimes / currentAccountTotalInfo.totalTradeTimes;
            break;
        }

        //======================================================== strategy info
        return {
          variety: item.name,
          label: resultLabel,
          percent: resultPercent,
        };
      });
    };

    const renderPinChart = (dataType: string, chartType: string) => {
      return (
        <Chart
          height={400}
          data={getVarietyProfitOrLossTimesChartData(dataType, chartType)}
          scale={{
            percent: {
              formatter: (val: any) => {
                val = (val * 100).toFixed(2) + "%";
                return val;
              },
            },
          }}
          autoFit
          onIntervalClick={(e: any) => {
            const states = e.target.cfg.element.getStates(); // 如果是选中，值为['selected'];取消选中，值为[]
          }}
        >
          <Coordinate type="theta" radius={0.75} />
          <Tooltip showTitle={false} />
          <Axis visible={false} />
          <Interval
            position="percent"
            adjust="stack"
            color="variety"
            style={{
              lineWidth: 1,
              stroke: "#fff",
            }}
            label={[
              "count",
              {
                // label 太长自动截断
                layout: {
                  type: "limit-in-plot",
                  cfg: { action: "ellipsis" },
                },
                content: (data) => getPinChartLabelPattern(chartType, data),
              },
            ]}
            state={{
              selected: {
                style: (t) => {
                  const res =
                    getTheme().geometries.interval.rect.selected.style(t);
                  return { ...res, fill: "black" };
                },
              },
            }}
          />
          <Interaction type="element-single-selected" />
        </Chart>
      );
    };

    //———————————————————————————————————————————————————————————— 渲染 ——————————————————————————————————————————————————————————————————————//
    return (
      <Fragment>
        <div>
          {/* ======================================================================== 基础信息 */}
          <div className="pb-[9px]">账号模式：{renderAccountType()}</div>
          <div>基础货币：RMB</div>
          <div>总交易单数：{currentAccountTotalInfo?.totalTradeTimes}</div>
          <div>总盈利次数：{currentAccountTotalInfo?.totalProfitTimes}</div>
          <div>总亏损次数：{currentAccountTotalInfo?.totalLossTimes}</div>
          <div>总净盈利：{currentAccountTotalInfo?.totalNetProfit}</div>
          <div>总胜率：{currentAccountTotalInfo?.totalWiningRate} %</div>

          <div>基础资金：{currentAccountTotalInfo?.baseAssets}</div>
          <div>剩余资金：{currentAccountTotalInfo?.baseAssets+currentAccountTotalInfo?.totalNetProfit}</div>
          <div>盈亏比率：{currentAccountTotalInfo?.winPercent} %</div>
          {/* ======================================================================== 品种盈利亏损次数 饼图*/}
          <div className="flex">
            <div className="flex-1">
              <div className="text-center">品种胜率</div>
              <div>{renderPinChart("variety", "winrate")}</div>
            </div>
            <div className="flex-1">
              <div className="text-center">品种交易次数</div>
              <div>{renderPinChart("variety", "count")}</div>
            </div>
          </div>
          {/* ======================================================================== 品种盈利亏损次数 饼图*/}
          {!!menuVarietyType && (
            <div>
              <div>{menuVarietyType}</div>
              <div className="flex">
                <div className="flex-1">
                  <div className="text-center">策略胜率</div>
                  <div>{renderPinChart("strategy", "winrate")}</div>
                </div>
                <div className="flex-1">
                  <div className="text-center">策略交易次数</div>
                  <div>{renderPinChart("strategy", "count")}</div>
                </div>
              </div>
            </div>
          )}
        </div>
      </Fragment>
    );
  }
);

export default ToTalPage;
