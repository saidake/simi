import React, { useState, useEffect } from "react";
export interface ITest {             //定义从父类接收的对象类型
  role?: string;
  isCps?: boolean;
}

const Test: React.FC<ITest> = ({ role, isCps }) => {          // 定义函数组件
  
  const [showDetails, setShowDetails] = useState<boolean | number>(false);       //  定义内部数据 和 修改函数  // setShowDetails(true)
  useEffect(() => {}, [showDetails]);       // 监听数据变化，数据发生变化才会执行函数
  useEffect(() => {
    let a: string[]=[];

  }, []);                  // 无状态组件内部使用，第一次渲染之后 和 每次更新之后都会执行，给无状态组件提供了类似生命周期函数的功能

  return <div>Test</div>;
};


export default Test;


// {
//   real: {
//     totalProfitTimes: 0,
//     totalLossTimes: 0,
//     totalTradeTimes: 0,
//     totalNetProfit: 0,
//     totalWiningRate: "",
//     varietyDataList: [{
//       totalProfitTimes: 0,
//       totalLossTimes: 0,
//       totalTradeTimes: 0,
//       totalNetProfit: 0,
//       totalWiningRate: "",
//       name: "",
//       code: ""
//     }]
//   },
//   simulated: {
//     totalProfitTimes: 0,
//     totalLossTimes: 0,
//     totalTradeTimes: 0,
//     totalNetProfit: 0,
//     totalWiningRate: "",
//     varietyDataList: [{
//       totalProfitTimes: 0,
//       totalLossTimes: 0,
//       totalTradeTimes: 0,
//       totalNetProfit: 0,
//       totalWiningRate: "",
//       name: "",
//       code: ""
//     }]
//   }
// }