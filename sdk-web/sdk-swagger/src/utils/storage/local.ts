/**
 *  保存LocalStorage
 *  @param    {String}    key
 *  @param    {String}    value
 */
export const SetLocalStorage = (key: string, value: any) => {
  localStorage.setItem(key, JSON.stringify(value));
};

/**
 *  获取LocalStorage
 *  @param    {String}    key
 *  @returns  {Object|String}
 */
export const GetLocalStorage = (key: string): any=> {
  if (!key) return;

  let value = localStorage.getItem(key);

  if (value) {
    return JSON.parse(value);
  }

  return '';
};

/**
 *  删除LocalStorage
 *  @param    {String}
 */
export const RemoveLocalStorage = (key: string) => {
  if (!key) return;

  localStorage.removeItem(key);
};

/**
 *  删除全部LocalStorage
 */
export const RemoveAllLocalStorage = () => {
  localStorage.clear();
};
