import { atom, RecoilState } from "recoil";

export interface IAccountData {
  totalTime: string;
  accountLinkedList: [
    {
      currencyType: string;
      startTime: string;
      endTime: string;
      turnover: string;
      purchasePrice: string;
      sellingPrice: string;
      totalProfitAndLoss: string;
      netProfitAndLoss: string;
      isBuy: boolean;
    }
  ];
}
export const accountData: RecoilState<IAccountData> = atom({
  key: "accountData",
  default: {
    totalTime: "",
    accountLinkedList: [
      {
        currencyType: "",
        startTime: "",
        endTime: "",
        turnover: "",
        purchasePrice: "",
        sellingPrice: "",
        totalProfitAndLoss: "",
        netProfitAndLoss: "",
        isBuy: Boolean(true),
      },
    ],
  },
});
