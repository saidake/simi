import React, { useEffect, useState } from "react";
import { fetchAccuntDataSync } from "./api/account";
import { accountData, IAccountData } from "./store/atoms/DashboardAtom";
import { useRecoilState } from "recoil";
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
} from "bizcharts";

const App: React.FC<any> = () => {
  const [accountRecoilData, setAccountRecoilData] =
    useRecoilState<IAccountData>(accountData);
  const [winningProbability, setWinningProbability] = useState(0);
  const [totalNetProfitAndLoss, setTotalNetProfitAndLoss] = useState(0);
  const [count, setCount] = useState(0);
  const [buyTimes, setBuyTimes] = useState(0);
  const [sellTimes, setSellTimes] = useState(0);
  const [dailyTradeAverageTimes, setDailyTradeAverageTimes] = useState<string>();
  const [dailyTradeEmptyTimes, setDailyTradeEmptyTimes] = useState<string>();
  const [endTimeChartData, setEndTimeChartData] = useState<
    { time: string; value: number }[]
  >([]);
  let testObj=new Set()


  useEffect(() => {
    getAccountData();
    testObj.add("dddd");
    testObj.add("dddd");
    testObj.add("dddd");
    testObj.add("ccc");
  }, []);
  useEffect(() => {
    console.log("testObj changed",testObj)
  }, [JSON.stringify(testObj)]);

  const getAccountData = async () => {
    let res: IAccountData = await fetchAccuntDataSync();
    setAccountRecoilData(res);
    setTotalNetProfitAndLoss(
      res.accountLinkedList.reduce(
        (prev, val) => prev + parseInt(val.netProfitAndLoss),
        0
      )
    );
    


    let successTimes = 0;
    let failedTimes = 0;
    let buyTimes = 0;
    let sellTimes = 0;
    let endTimeMap = new Map();
    let resultEndTimeChartData: { time: string; value: number }[] = [];
    const timeReg = /\d{4}\/\d{1,2}\/\d{1,2}/;
    res.accountLinkedList.forEach((val) => {
      if (val.isBuy) {
        buyTimes++;
      } else {
        sellTimes++;
      }
      if (parseInt(val.netProfitAndLoss) > 0) {
        successTimes += 1;
      } else {
        failedTimes += 1;
      }
      const result = timeReg.exec(val.endTime);
      let regTimeContent = !!result ? result[0] : null;
      let mapTimeValue = endTimeMap.get(regTimeContent);
      if (mapTimeValue === undefined) {
        endTimeMap.set(regTimeContent, parseFloat(val.netProfitAndLoss));
      } else {
        endTimeMap.set(
          regTimeContent,
          parseFloat(val.netProfitAndLoss) + mapTimeValue
        );
      }
    });
    endTimeMap.forEach((val, key) => {
      resultEndTimeChartData.push({
        time: key,
        value: val,
      });
    });

    setCount(res.accountLinkedList.length);
    setWinningProbability(
      parseFloat((successTimes / (failedTimes + successTimes)).toFixed(4))
    );
  console.log("endTimeMap.values.length",endTimeMap.size)
  console.log("res.accountLinkedList.length",res.accountLinkedList.length)

    setDailyTradeAverageTimes((res.accountLinkedList.length/endTimeMap.size).toFixed(2))
    setBuyTimes(buyTimes);
    setSellTimes(sellTimes);
    setEndTimeChartData(resultEndTimeChartData);
    return res;
  };

  return (
    <div>
      <div>{accountRecoilData.totalTime}</div>
      <div>基础货币：USD</div>
      <br />
      <div>总交易单数：{count}</div>
      <div>日均交易单数：{dailyTradeAverageTimes}</div>
      <div>买入单数：{buyTimes}</div>
      <div>卖出单数：{sellTimes}</div>
      <div>总净盈亏：{totalNetProfitAndLoss}</div>
      <div>胜率：{(winningProbability * 100).toFixed(2)}%</div>
      <br />
      <br />
      <div style={{ width: 700 }}>
        <Chart
          padding={[30, 20, 50, 70]}
          autoFit
          height={500}
          data={endTimeChartData}
          scale={{
            value:{
              min:-100,
              max:100,
              tickInterval:10
            },
            time:{
              tickInterval:1
            }
          }}
        >
          <Axis name="time" label={{
            style:{
              fontSize:10,
              fill: '#404040', // 文本的颜色
              autoHide:false
            },
            formatter(text:string,item,index) {
              let arr = /\d{1,2}-\d{1,2}$/.exec(text);
              return !arr?"[error]":arr[0];
            }
          }}/>
          <Axis name="value" />
          <Interaction type="active-region" />

          <Tooltip showCrosshairs showMarkers={false} />
          <Interval
            position="time*value"
            color={[
              "time*value",
              (time, value) => {
                return value>0 ? "#36c361" : "#ff5957";
              },
            ]}
            label={[
              "value",val=>{
                return {
                  content: val.toFixed(2)
                }
              }
            ]}
          />
          <Legend visible={false} />
        </Chart>
      </div>
    </div>
  );
};

export default App;
