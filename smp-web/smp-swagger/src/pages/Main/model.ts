import {
    getApiDocs,
    getSwaggerResourceList
} from '@/services/swagger';

const lodash = require("lodash");


export default {
    namespace: 'main',
    state: {
        // apiInfoMap: {},
        apiDocList: [],
        swaggerResourceList: []
    },

    effects: {
        *getSwaggerResourceList({ payload,callback }, { call, put }) {
            const returnSwaggerResourceList = yield call(getSwaggerResourceList);
            console.log("[Main] 初始化==>获取的returnSwaggerResourceList：",returnSwaggerResourceList)
            if (returnSwaggerResourceList&&returnSwaggerResourceList.length>0) {
                const returnApiDocList=[];
                for(let i=0;i<returnSwaggerResourceList.length;i++){
                    const currentResource=returnSwaggerResourceList[i]
                    if(currentResource){
                        const returnDocMap = yield call(getApiDocs, currentResource?.location);
                        returnApiDocList.push(returnDocMap)
                    }
                }
                yield put({
                    type: "saveData",
                    payload: {
                        swaggerResourceList: returnSwaggerResourceList,
                        apiDocList:returnApiDocList
                    }
                })
                yield callback&&callback(returnApiDocList)
            }
        },
        *handleMenuSelect({ payload,callback }, { call, put }) {
            // const apiInfoMap=this.state.apiInfoMap;
            // console.log("apiInfoMap",apiInfoMap)
            const {menuData,tabList,isTempTab,renderTabItem,key,renderTabTitle}=payload;
            let currentMenuData=menuData[key];
            const activeTabKey = currentMenuData.getObj?.operationId
            const newTabList = [...tabList];
            if (isTempTab) newTabList.pop()
            newTabList.push({
              title: renderTabTitle(currentMenuData),
              content: renderTabItem({...currentMenuData}),
              key: activeTabKey
            })
            yield callback(newTabList,activeTabKey)
        },
        // *getApiDocs({ payload,callback }, { call, put }) {
        //     const returndata = yield call(getApiDocs, payload);
        //     if (returndata) {
        //         yield put({
        //             type: "saveData",
        //             payload: {
        //                 apiInfoMap: returndata
        //             }
        //         })
        //         yield callback&&callback(returndata)
        //     }
        // },

    },
    reducers: {
        saveData(state, { payload }) {
            console.log('payload: ', payload);
            return { ...state, ...payload };
        }
    }
}



