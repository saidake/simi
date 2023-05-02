import React, { Fragment, useEffect, useState } from "react";
import {
  accountData,
  IAccountResponseData,
  IBaseAccountInfo,
} from "@/store/atoms/DashboardAtom";
import ToTalPage from "@pages/qihuo/TotalPage";
import { fetchAccuntDataSync } from "@/api/account";
import {
  HomeOutlined,
  RadarChartOutlined,
  HeatMapOutlined,
} from "@ant-design/icons";
import classNames from "classnames";
import { useRecoilState, useRecoilStateLoadable } from "recoil";
import { Spin, Button } from "antd";

const BaseLayout: React.FC<any> = () => {
  const [accountRecoilDataLoadable, setAccountRecoilData] =
    useRecoilStateLoadable<IAccountResponseData>(accountData);
  const [menuIconIndex, setMenuIconIndex] = useState<number>(0); //  定义内部数据 和 修改函数  // setShowDetails(true)
  const [menuVarietyType, setMenuVarietyType] = useState<string>(""); //  定义内部数据 和 修改函数  // setShowDetails(true)

  //———————————————————————————————————————————————————————————— 生命周期方法 ——————————————————————————————————————————————————————————————————————//
  useEffect(() => {
    getAccountData();
  }, []);

  //———————————————————————————————————————————————————————————— 数据处理方法 ——————————————————————————————————————————————————————————————————————//
  const renderCurrentAccountVariety = () => {
    if(accountRecoilDataLoadable.state!=="hasValue"){
      return <Spin className="absolute top-[50%] left-[50%] translate-x-[-50%] translate-y-[-50%]" />
    }
    let currentVarietyAccountInfo:IBaseAccountInfo|null=null;
    switch (menuIconIndex) {
      case 0:
        currentVarietyAccountInfo= accountRecoilDataLoadable.contents.general;
        break;
      case 1:
        currentVarietyAccountInfo= accountRecoilDataLoadable.contents.simulated;
        break;
      case 2:
        currentVarietyAccountInfo= accountRecoilDataLoadable.contents.real;
        break;
      default:
        return;
    }
    let varietyDataMap= currentVarietyAccountInfo.varietyDataMap;
    return Object.keys(varietyDataMap).map((item) => (
      <div className="w-full h-[36px] flex items-center justify-center" key={item} onClick={()=>{
        setMenuVarietyType(item)
      }}>
        {item}
      </div>
    ))

  };
  const getAccountData = async () => {
    setAccountRecoilData(await fetchAccuntDataSync());
  };

  const renderLoadable = (content: any) => {
    if (accountRecoilDataLoadable.state === "hasValue") {
      return content;
    } else if (accountRecoilDataLoadable.state === "loading") {
      return (
        <Spin className="absolute top-[50%] left-[50%] translate-x-[-50%] translate-y-[-50%]" />
      );
    } else if (accountRecoilDataLoadable.state === "hasError") {
      return <div>load error</div>;
    }
  };
  //———————————————————————————————————————————————————————————— 渲染 ——————————————————————————————————————————————————————————————————————//
  return (
    <div className="bg-mainpage-bg text-mainpage-font h-full flex flex-col">
      {/* ========================================================================================== 导航栏 */}
      <div className="bg-mainpage-container h-[38px] shrink-0 min-h-[38px] mb-[4px] pl-[10px] pt-[6px]">
        TOP
      </div>
      <div className="flex flex-1 shrink-1 overflow-y-hidden">
        {/* ========================================================================================== 左侧菜单 */}
        <div className="basis-[52px] flex-0 shrink-0 bg-mainpage-container mr-[4px] rounded-tr-[4px]">
          {/* ========================================================================== 菜单图标 */}
          <div className="pt-[6px] pb-[6px] text-[22px]">
            <div
              className={classNames(
                "w-full h-[36px] flex items-center justify-center",
                {
                  "text-mainpage-selected": menuIconIndex === 0,
                }
              )}
              onClick={() => setMenuIconIndex(0)}
            >
              <HomeOutlined />
            </div>
            <div
              className={classNames(
                "w-full h-[36px] flex items-center justify-center",
                {
                  "text-mainpage-selected": menuIconIndex === 1,
                }
              )}
              onClick={() => setMenuIconIndex(1)}
            >
              <RadarChartOutlined />
            </div>
            <div
              className={classNames(
                "w-full h-[36px] flex items-center justify-center",
                {
                  "text-mainpage-selected": menuIconIndex === 2,
                }
              )}
              onClick={() => setMenuIconIndex(2)}
            >
              <HeatMapOutlined />
            </div>
          </div>
          {/* ========================================================================== 品种图标 */}
          <div className="pt-[6px] pb-[6px] text-sm cursor-pointer">
            {renderCurrentAccountVariety()}
          </div>
        </div>
        {/* ========================================================================================== 右侧菜单 */}
        <div className="flex-1 shrink-1 rounded-tl-[4px] bg-mainpage-container pl-[19px] pt-[8px] overflow-y-auto overflow-x-hidden">
          {renderLoadable(<ToTalPage menuIconIndex={menuIconIndex} menuVarietyType={menuVarietyType} />)}
        </div>
      </div>
    </div>
  );
};

export default BaseLayout;
