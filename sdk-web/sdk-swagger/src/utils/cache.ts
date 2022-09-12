import { SetLocalStorage, GetLocalStorage, RemoveLocalStorage, RemoveAllLocalStorage } from './storage/local';
import { SetSessionStorage, GetSessionStorage, RemoveSessionStorage, RemoveAllSessionStorage } from './storage/session';

interface AccountInfo {
  menu: any[]
}

// 用户token
export const SetGlobalToken = (token: string) => SetSessionStorage('globalToken', token);
export const GetGlobalToken= () => GetSessionStorage('globalToken');
export const RemoveGlobalToken = () => RemoveSessionStorage('globalToken');


// 用户信息
export const SetAccountInfo = (info: AccountInfo) => SetSessionStorage('accountInfo', info);
export const GetAccountInfo= () => GetSessionStorage('accountInfo');
export const RemoveAccountInfo = () => RemoveSessionStorage('accountInfo');

// 城市信息
export const SetRegionJson = (regionJson: any[]) => SetLocalStorage('regionJson', regionJson);
export const GetRegionJson = () => GetLocalStorage('regionJson');
export const RemoveRegionJson = () => RemoveLocalStorage('regionJson');

// app切换列表
export const SetAppList = (appList: any[]) => SetSessionStorage('appList', appList);
export const GetAppList = () => GetSessionStorage('appList');
export const RemoveAppList = () => RemoveSessionStorage('appList');

// 当前app应用信息
export const SetCurrentApp = (currentAppId: number) => SetSessionStorage('currentAppId', currentAppId);
export const GetCurrentApp = () => GetSessionStorage('currentAppId');
export const RemoveCurrentApp = () => RemoveSessionStorage('currentAppId');

//当前应用积分

export const SetIntegral = (integral: number) => SetSessionStorage('integral', integral);
export const GetIntegral = () => GetSessionStorage('integral');
export const RemoveIntegral = () => RemoveSessionStorage('integral');

// 当前页面缓存
// sessionStorage.setItem(key, JSON.stringify(value));
export const SetCacheValue = (cacheValue:any)=> sessionStorage.setItem('cacheValue',JSON.stringify(cacheValue));
export const GetCacheValue = ()=> JSON.parse(sessionStorage.getItem('cacheValue'));
export const RemoveCacheValue = ()=> sessionStorage.removeItem('cacheValue');


export const RemoveAllStorage = () => {
  RemoveAllLocalStorage();
  RemoveAllSessionStorage();
};
