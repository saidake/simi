import React, { useState, useEffect, Suspense } from "react";
const Test=React.lazy(()=>import("@pages/Test"))    //懒加载，只有使用到Test组件时才会加载，必须配合Suspense使用

export interface ITest {             //定义从父类接收的对象类型
  role: string;
  isCps?: boolean;
  onUpdateUser: (params: any) => void;
}


export const StdReact: React.FC<ITest> = ({ role, isCps }) => {          // 定义函数组件
  
  const [showDetails, setShowDetails] = useState<boolean | number>(false);       //  定义内部数据 和 修改函数  // setShowDetails(true)
  useEffect(() => {}, [showDetails]);       // 监听数据变化，数据发生变化才会执行函数
  useEffect(() => {}, []);                  // 无状态组件内部使用，第一次渲染之后 和 每次更新之后都会执行，给无状态组件提供了类似生命周期函数的功能
  return <div>
    <Suspense fallback={<div>loading...</div>}>    {/* 加载时显示的组件 */}
      <Test />
    </Suspense>
  </div>
};
