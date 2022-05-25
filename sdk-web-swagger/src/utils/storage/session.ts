/**
 *  保存SessionStorage
 *  @param    {String}    key
 *  @param    {String}    value
 */
export const SetSessionStorage = (key: string, value: any = '') => {
  if (!key) return;

  sessionStorage.setItem(key, JSON.stringify(value));
};

/**
 *  获取SessionStorage
 *  @param    {String}    key
 *  @returns  {Object|String}
 */
export const GetSessionStorage = (key: string) => {
  if (!key) return;

  let value = sessionStorage.getItem(key);

  if (value) {
    return JSON.parse(value);
  }

  return '';
};

/**
 *  删除SessionStorage
 *  @param    {String}
 */
export const RemoveSessionStorage = (key: string) => {
  if (!key) return;

  sessionStorage.removeItem(key);
};

/**
 *  删除全部SessionStorage
 */
export const RemoveAllSessionStorage = () => {
  sessionStorage.clear();
};
