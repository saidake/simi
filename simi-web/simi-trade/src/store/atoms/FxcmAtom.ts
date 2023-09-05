import { atom, RecoilState } from "recoil";
export interface IAccountLinkedData{
  currencyType: string,
  startTime: string,
  endTime: string,
  turnover: string,
  purchasePrice: string,
  sellingPrice: string,
  totalProfitAndLoss: string,
  netProfitAndLoss: string,
  isBuy: boolean
}

export interface IFxcmAccount {
  accountLinkedList: IAccountLinkedData[]
}
export const fxcmAccount: RecoilState<IFxcmAccount> = atom({
  key: "fxcmAccount"
});
