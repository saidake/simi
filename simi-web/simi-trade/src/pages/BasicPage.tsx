import React, { Fragment, useEffect, useState, useContext } from "react";
import {
  HomeOutlined,
  RadarChartOutlined,
  HeatMapOutlined,
} from "@ant-design/icons";
import classNames from "classnames";
import Fxcm from "./Fxcm/Fxcm";
import StockChartPage from "./Stock/StockChartPage";

const BasicPage: React.FC<any> = ({children}) => {
  const [menuIconIndex, setMenuIconIndex] = useState<number>(0); 

  const renderRightContent=(menuIconIndex:number)=>{
    switch(menuIconIndex){
      // case 0: return <Fxcm/>
      case 0: return <StockChartPage/>
      case 1: return <StockChartPage/>
      case 2: return <div>test 2 page</div>
    }
  }

  //———————————————————————————————————————————————————————————— Render ——————————————————————————————————————————————————————————————————————//
  return (
    <div className="bg-mainpage-bg text-mainpage-font h-full flex flex-col">
      {/* START========================================================================================== [0] Navigator */}
      <div className="bg-mainpage-container h-[38px] shrink-0 min-h-[38px] mb-[4px] pl-[10px] pt-[6px]">
        TOP Navigator
      </div>
      {/* END  ========================================================================================== [0] Navigator */}
      {/* START========================================================================================== [0] Bottom Box */}
      <div className="flex flex-1 shrink-1 overflow-y-hidden">
        {/* START========================================================================================== [1] Left Menu */}
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
        </div>
        {/* END  ========================================================================================== [1] Left Menu */}
        {/* START========================================================================================== [1] Right Content */}
        <div className="flex-1 shrink-1 rounded-tl-[4px] bg-mainpage-container pl-[19px] pt-[8px] overflow-y-auto overflow-x-hidden">
          {/* {renderLoadable(<ToTalPage menuIconIndex={menuIconIndex} menuVarietyType={menuVarietyType} />)} */}
          {renderRightContent(menuIconIndex)}
        </div>
        {/* END  ========================================================================================== [1] Right Content */}
      </div>
      {/* END  ========================================================================================== [0] Bottom Box */}
    </div>
  );
};

export default BasicPage;
