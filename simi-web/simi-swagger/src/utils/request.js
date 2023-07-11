const fetch = require("dva").fetch;
import { notification } from 'antd';
import {history} from 'umi';
import { GetGlobalToken, RemoveAllStorage } from './cache';
import { Utils } from './utils';

const codeMessage = {
  200: '服务器成功返回请求的数据。',
  201: '新建或修改数据成功。',
  202: '一个请求已经进入后台排队（异步任务）。',
  204: '删除数据成功。',
  400: '发出的请求有错误，服务器没有进行新建或修改数据的操作。',
  401: '用户没有权限（令牌、用户名、密码错误）。',
  403: '用户得到授权，但是访问是被禁止的。',
  404: '发出的请求针对的是不存在的记录，服务器没有进行操作。',
  406: '请求的格式不可得。',
  410: '请求的资源被永久删除，且不会再得到的。',
  422: '当创建一个对象时，发生一个验证错误。',
  500: '服务器发生错误，请检查服务器。',
  502: '网关错误。',
  503: '服务不可用，服务器暂时过载或维护。',
  504: '网关超时。',
};

const checkStatus = (response) => {
  if (response.status >= 200 && response.status < 300) {
    return response;
  }
  const errortext = codeMessage[response.status] || response.statusText;
  notification.error({
    // message: `请求错误 ${response.status}: ${response.url}`,
    message: errortext,
    // description: errortext,
    duration: 2,
  });
  const error = new Error(errortext);
  error.name = response.status;
  error.response = response;
  throw error;
};

const checkResult = (response) => {
  const contentType = response.headers.get('Content-Type');
  if (contentType && contentType.match(/application\/json/i)) {
    response
      .clone()
      .json()
      .then((content) => {
        const code = Number(content.code);
        try {
          if (code !== 0) {
            switch (code) {
            case 103:
              notification.error({
                message: content.message,
                // description: content.message,
                duration: 2,
              });

              RemoveAllStorage();
              history.push('/login');
              break;
            default:
              throw content.message || content.msg;
            }
          }
        } catch (error) {
          notification.error({
            message: error,
            // description: content.message,
          });
        }
      });
  }

  return response;
};

export default function request(url, option) {
  let _url = '';
  let token = GetGlobalToken();

  const defaultOptions = {
    // credentials: 'include',
    credentials: 'omit',
  };

  let options = {
    ...option,
  };

  const headers = Object.assign({}, token ? { token } : {});

  const newOptions = { ...defaultOptions, ...options, headers };
  // console.log(newOptions, "newOptions");
  if (
    newOptions.method === 'POST' ||
    newOptions.method === 'PUT' ||
    newOptions.method === 'DELETE'
  ) {
    _url = url;
    if (!(newOptions.body instanceof FormData)) {
      newOptions.headers = {
        Accept: 'application/json',
        'Content-Type': 'application/json; charset=utf-8',
        ...newOptions.headers,
      };
      newOptions.body = JSON.stringify(newOptions.body);
    } else {
      // newOptions.body is FormData
      newOptions.headers = {
        Accept: 'application/json',
        ...newOptions.headers,
      };
    }
  } else if (newOptions.method === 'IMAGE') {
    _url = `${url}${
      Utils.IsEmptyObject(options.params)
        ? ''
        : '?' + Utils.ObjectToUrl({ params: JSON.stringify(options.params) })
    }`;
    newOptions.method = 'GET';
  } else {
    _url = `${url}${
      Utils.IsEmptyObject(options)
        ? ''
        : '?' + Utils.ObjectToUrl({ params: JSON.stringify(options) })
    }`;
  }
  // console.log('_url: ', _url);
  return fetch(_url, newOptions)
    .then(checkStatus)
    .then((response) => checkResult(response))
    .then((response) => {
      // DELETE and 204 do not return data by default
      // using .json will report an error.
      if (newOptions.method === 'DELETE' || response.status === 204) {
        return response.text();
      } else if (options.method === 'IMAGE') {
        return response.blob();
      }

      return response.json();
    })
    .catch((e) => {
      console.log(e);
    });
}
