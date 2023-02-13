// import React, { Fragment, useEffect, useState } from "react";
// import { fetchAccuntDataSync } from "./api/account";
// import { accountData, IAccountData } from "./store/atoms/DashboardAtom";
// import { useRecoilState } from "recoil";
// import {
//   Chart,
//   Line,
//   Point,
//   Tooltip,
//   Axis,
//   Interaction,
//   Legend,
//   Interval,
//   Annotation,
// } from "bizcharts";

// const App: React.FC<any> = () => {
//   const [accountRecoilData, setAccountRecoilData] =
//     useRecoilState<IAccountData>(accountData);
//   const [winningProbability, setWinningProbability] = useState(0);
//   const [winningProbabilityEURUSD, setWinningProbabilityEURUSD] = useState(0);
//   const [winningProbabilityUSDJPY, setWinningProbabilityUSDJPY] = useState(0);
//   const [winningProbabilityUSDCNH, setWinningProbabilityUSDCNH] = useState(0);
//   const [totalNetProfitAndLoss, setTotalNetProfitAndLoss] = useState(0);
//   const [count, setCount] = useState(0);
//   const [buyTimes, setBuyTimes] = useState(0);
//   const [sellTimes, setSellTimes] = useState(0);
//   const [dailyTradeAverageTimes, setDailyTradeAverageTimes] =
//     useState<string>();
//   const [endTimeChartData, setEndTimeChartData] = useState<
//     { time: string; value: number }[]
//   >([]);
//   let testObj = new Set();

//   useEffect(() => {
//     getAccountData();
//     testObj.add("dddd");
//     testObj.add("dddd");
//     testObj.add("dddd");
//     testObj.add("ccc");
//   }, []);
//   useEffect(() => {
//     console.log("testObj changed", testObj);
//   }, [JSON.stringify(testObj)]);

//   const getAccountData = async () => {
//     let res: IAccountData = await fetchAccuntDataSync();
//     console.log("res.accountLinkedList", res.accountLinkedList);
//     res.accountLinkedList = res.accountLinkedList.filter(
//       (item) => item.currencyType != "TRY/JPY" && item.currencyType != "USD/JPY"
//     );
//     setAccountRecoilData(res);
//     setTotalNetProfitAndLoss(
//       res.accountLinkedList.reduce(
//         (prev, val) => prev + parseInt(val.netProfitAndLoss),
//         0
//       )
//     );
//     let successTimes = 0;
//     let failedTimes = 0;
//     let successTimesEURUSD = 0;
//     let failedTimesEURUSD = 0;
//     let successTimesUSDJPY = 0;
//     let failedTimesUSDJPY = 0;
//     let successTimesUSDCNH = 0;
//     let failedTimesUSDCNH = 0;
//     let buyTimes = 0;
//     let sellTimes = 0;
//     let endTimeMap = new Map();
//     let resultEndTimeChartData: { time: string; value: number }[] = [];
//     const timeReg = /\d{4}\/\d{1,2}\/\d{1,2}/;
//     res.accountLinkedList.forEach((val) => {
//       // buy times
//       if (val.isBuy) {
//         buyTimes++;
//       } else {
//         sellTimes++;
//       }
//       // win probability
//       if (parseInt(val.netProfitAndLoss) > 0) {
//         if (val.currencyType === "EUR/USD") successTimesEURUSD += 1;
//         if (val.currencyType === "USD/JPY") successTimesUSDJPY += 1;
//         if (val.currencyType === "USD/CNH") successTimesUSDCNH += 1;
//         successTimes += 1;
//       } else {
//         if (val.currencyType === "EUR/USD") failedTimesEURUSD += 1;
//         if (val.currencyType === "USD/JPY") failedTimesUSDJPY += 1;
//         if (val.currencyType === "USD/CNH") failedTimesUSDCNH += 1;
//         failedTimes += 1;
//       }

//       const result = timeReg.exec(val.endTime);
//       let regTimeContent = !!result ? result[0] : null;
//       let mapTimeValue = endTimeMap.get(regTimeContent);
//       if (mapTimeValue === undefined) {
//         endTimeMap.set(regTimeContent, parseFloat(val.netProfitAndLoss));
//       } else {
//         endTimeMap.set(
//           regTimeContent,
//           parseFloat(val.netProfitAndLoss) + mapTimeValue
//         );
//       }
//     });
//     endTimeMap.forEach((val, key) => {
//       resultEndTimeChartData.push({
//         time: key,
//         value: val,
//       });
//     });

//     setCount(res.accountLinkedList.length);
//     setWinningProbability(
//       parseFloat((successTimes / (failedTimes + successTimes)).toFixed(4))
//     );
//     setWinningProbabilityEURUSD(
//       parseFloat(
//         (successTimesEURUSD / (failedTimesEURUSD + successTimesEURUSD)).toFixed(
//           4
//         )
//       )
//     );
//     setWinningProbabilityUSDJPY(
//       parseFloat(
//         (successTimesUSDJPY / (failedTimesUSDJPY + successTimesUSDJPY)).toFixed(
//           4
//         )
//       )
//     );
//     setWinningProbabilityUSDCNH(
//       parseFloat(
//         (successTimesUSDCNH / (failedTimesUSDCNH + successTimesUSDCNH)).toFixed(
//           4
//         )
//       )
//     );

//     setDailyTradeAverageTimes(
//       (res.accountLinkedList.length / endTimeMap.size).toFixed(2)
//     );
//     setBuyTimes(buyTimes);
//     setSellTimes(sellTimes);
//     setEndTimeChartData(resultEndTimeChartData);
//     return res;
//   };

//   return (
//     <div className="bg-mainpage-bg text-mainpage-color h-full p-6">
//       {/* ======================================================================== 基础信息 */}
//       <div>
//         <div>{accountRecoilData.totalTime}</div>
//         <div>基础货币：RMB</div>
//         <br />
//         <div>总交易单数：{count}</div>
//         <div>日均交易单数：{dailyTradeAverageTimes}</div>
//         <div>买入单数：{buyTimes}</div>
//         <div>卖出单数：{sellTimes}</div>
//         <div>总净盈亏：{totalNetProfitAndLoss}</div>
//         <br />
//         <div>综合胜率：{(winningProbability * 100).toFixed(2)}%</div>
//         <div>EURUSD胜率：{(winningProbabilityEURUSD * 100).toFixed(2)}%</div>
//         <div>USDJPY胜率：{(winningProbabilityUSDJPY * 100).toFixed(2)}%</div>
//         <div>USDCNH胜率：{(winningProbabilityUSDCNH * 100).toFixed(2)}%</div>
//         <br />
//         <br />
//       </div>
//       {/* ======================================================================== 图表 */}
//       <div style={{ width: 700 }}>
//         <Chart
//           padding={[30, 20, 50, 0]}
//           autoFit
//           height={500}
//           data={endTimeChartData}
//           scale={{
//             value: {
//               min: -100,
//               max: 100,
//               tickInterval: 10,
//             },
//             time: {
//               tickInterval: 1,
//             },
//           }}
//         >
//           <Axis
//             name="time"
//             label={{
//               style: {
//                 fontSize: 10,
//                 fill: "#404040", // 文本的颜色
//                 autoHide: false,
//               },
//               formatter(text: string, item, index) {
//                 let arr = /\d{1,2}-\d{1,2}$/.exec(text);
//                 return !arr ? "[error]" : arr[0];
//               },
//             }}
//           />
//           <Axis name="value" />
//           <Interaction type="active-region" />

//           <Tooltip showCrosshairs showMarkers={false} />
//           <Interval
//             position="time*value"
//             color={[
//               "time*value",
//               (time, value) => {
//                 return value > 0 ? "#36c361" : "#ff5957";
//               },
//             ]}
//             label={[
//               "value",
//               (val) => {
//                 return {
//                   content: val.toFixed(2),
//                 };
//               },
//             ]}
//           />
//           <Legend visible={false} />
//         </Chart>
//       </div>
//     </div>
//   );
// };

// export default App;
export {}