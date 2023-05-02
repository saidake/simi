import { atom, RecoilState } from "recoil";

//======================================================= /qihuo/account
export interface ITotalInfo {
  totalProfitTimes: number,
  totalLossTimes: number,
  totalTradeTimes: number,
  totalNetProfit: number,
  totalWiningRate: string,
  name: string,
}
export interface IStragetyMap extends ITotalInfo {
    rule: string,
}

export interface IVarietyMap extends ITotalInfo{
    code: string,
    strategyDataMap: {
      [xxx:string]:IStragetyMap
    },
}
export interface IBaseAccountInfo {
  totalProfitTimes: number,
  totalLossTimes: number,
  totalTradeTimes: number,
  totalNetProfit: number,
  totalWiningRate: string,
  baseAssets: number,
  winPercent: string,
  varietyDataMap: {
    [xxx:string]:IVarietyMap
  }
}

export interface IAccountResponseData {
  general: IBaseAccountInfo,
  real: IBaseAccountInfo,
  simulated: IBaseAccountInfo
}
export const accountData: RecoilState<IAccountResponseData> = atom({
  key: "accountData"
});
