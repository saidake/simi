import { parse, stringify } from 'qs';
import moment from 'moment';
class Utils {
  public static fixedZero(val) {
    return val * 1 < 10 ? `0${val}` : val;
  }

  public static IsUrl(path: string): boolean {
    const reg = /(((^https?:(?:\/\/)?)(?:[-;:&=\+\$,\w]+@)?[A-Za-z0-9.-]+(?::\d+)?|(?:www.|[-;:&=\+\$,\w]+@)[A-Za-z0-9.-]+)((?:\/[\+~%\/.\w-_]*)?\??(?:[-\+=&;%@.\w_]*)#?(?:[\w]*))?)$/;
    
    return reg.test(path)
  }

  public static IsEmptyObject(obj: object): boolean {
    if (!obj) return;

    let isEmpty = true;

    for (let i in obj) {
      isEmpty = false;
      break;
    }

    return isEmpty;
  }

  public static ObjectToUrl(obj: any): string {
    return Object.keys(obj).map(key => {
      if (obj[key] !== undefined && obj[key] !== null) {
        return key + '=' + encodeURIComponent(obj[key]);
      }
    }).join('&'); 
  }

  public static MergeObject(objArr: any[]): object {
    return objArr ? Object.assign({}, ...objArr) : {};
  }

  public static GetPageQuery(): any {
    return parse(window.location.href.split('?')[1]);
  }

  // 根据类型获取页面Title
  public static GetPageTitleByMode(mode: string, modulesName: string):string {
    switch(mode) {
      case 'create':
        return `新增${modulesName}`
      case 'edit':
        return `编辑${modulesName}`
      case 'detail':
        return `${modulesName}详情`
      default:
        throw new Error(`unknow mode: ${mode}`);
    }
  };

   // 根据类型获取页面Title
   public static GetPageManageTitleByMode(mode: string, modulesName: string):string {
    switch(mode) {
      case 'create':
        return `新增${modulesName}`
      case 'edit':
        return `编辑${modulesName}`
      case 'check':
        return `${modulesName}详情`
      default:
        throw new Error(`unknow mode: ${mode}`);
    }
  };

  public static BlobToBase64 = async blob => {
    if (!blob) return;

    return new Promise(resolve => {
      let reader = new FileReader();
      reader.readAsDataURL(blob);
      reader.onload = () => {
        resolve(reader.result);
      };
    });
  }



  /**
   * 转换数字为中国金额格式
   *
   * @static
   * @param {number} amount
   * @returns {string}
   * @memberof Utils
   */
  public static NumberFormatAmount(amount: number): string {
    const options = { style: 'currency', currency: 'CNY' };
    const numberFormat = new Intl.NumberFormat('hanidec', options);

    return numberFormat.format(amount);
  };

  public static JsonToUrl = (json: { [x: string]: string; }) => {
    if (!json) throw new Error('json对象未定义或为空');
  
    return Object.keys(json).map((key, index) => {
      if (json[key] !== null && json[key] !== undefined && json[key] !== '') {
        return key + '=' + encodeURIComponent(json[key]);
      }
    }).join('&');
  };

  public static getPageQuery(): any {
    return parse(window.location.href.split('?')[1]);
  };

  public static spliceDownloadUrl(url, params): any  {
    return `${url}${!this.IsEmptyObject(params) ? '?' + this.ObjectToUrl({ params: JSON.stringify(params) }) : ''}`;
  }

  public static getTimeDistance(type: string): any[] {
    const now = new Date();
    const oneDay = 1000 * 60 * 60 * 24;

    if (type === 'today') {
      now.setHours(0);
      now.setMinutes(0);
      now.setSeconds(0);
      return [moment(now), moment(now.getTime() + (oneDay - 1000))];
    }

    if (type === 'yesterday') {
      now.setHours(0);
      now.setMinutes(0);
      now.setSeconds(0);

      return [moment(now.getTime() - oneDay), moment(now.getTime() + (oneDay - 1000) - oneDay)];
    }

    if (type === 'week') {
      // let day = now.getDay();
      now.setHours(0);
      now.setMinutes(0);
      now.setSeconds(0);

      // if (day === 0) {
      //   day = 6;
      // } else {
      //   day -= 1;
      // }

      const beginTime = now.getTime() - 7 * oneDay;

      return [moment(beginTime), moment(now.getTime() + (oneDay - 1000))];
    }

    if (type === 'month') {
      now.setHours(0);
      now.setMinutes(0);
      now.setSeconds(0);

      const beginTime = now.getTime() - 30 * oneDay;

      return [moment(beginTime), moment(now.getTime() + (oneDay - 1000))];
    }

    // if (type === 'month') {
    //   const year = now.getFullYear();
    //   const month = now.getMonth();
    //   const nextDate = moment(now).add(1, 'months');
    //   const nextYear = nextDate.year();
    //   const nextMonth = nextDate.month();

    //   return [
    //     moment(`${year}-${this.fixedZero(month + 1)}-01 00:00:00`),
    //     moment(moment(`${nextYear}-${this.fixedZero(nextMonth + 1)}-01 00:00:00`).valueOf() - 1000),
    //   ];
    // }

    const year = now.getFullYear();
    return [moment(`${year}-01-01 00:00:00`), moment(`${year}-12-31 23:59:59`)];
  }
}

export { Utils };
