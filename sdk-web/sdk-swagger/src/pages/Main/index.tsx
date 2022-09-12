import { connect } from 'umi';
import React, { PureComponent, Dispatch } from 'react';
import { ConfigProvider, Layout, Modal, message, Tabs, Dropdown, Button, Space, Tooltip, Form, Menu, MenuProps, Spin, Select,Table } from 'antd';
import { UserOutlined, DownOutlined, LaptopOutlined, NotificationOutlined, DoubleLeftOutlined, DoubleRightOutlined } from '@ant-design/icons';
import { Input } from 'antd';
const FormItem = Form.Item;
import { history } from 'umi';
import styles from './index.less';
const { Header, Footer, Sider, Content } = Layout;
const lodash = require("lodash");
import logo from "@/assets/daboluo-logo.svg"
const { TabPane } = Tabs;
const { Option } = Select;
import JSONInput from 'react-json-editor-ajrm';
import locale    from 'react-json-editor-ajrm/locale/zh-cn';

interface IndexProps {
  loading: boolean;
  main: any;
  dispatch: Dispatch<any>;
  apiInfoMap: any[];
  swaggerResourceList: any[];
}

const columns = [
  {
    title: '参数名称',
    dataIndex: 'name',
    key: 'name',
  },
  {
    title: '参数说明',
    dataIndex: 'description',
    key: 'description',
    width: '12%',
  },
  {
    title: '请求类型',
    dataIndex: 'requestType',
    // width: '30%',
    key: 'requestType',

  },
  {
    title: '是否必须',
    dataIndex: 'required',
    key: 'required',
    render:(text, record, index)=>text?<span style={{color:"#f00"}}>true</span>:<span>false</span>
  },
  {
    title: '数据类型',
    dataIndex: 'dataType',
    key: 'dataType',
  },
  {
    title: '引用实体',
    dataIndex: 'schema',
    key: 'schema',
  },
];

class IndexPage extends PureComponent<IndexProps, any> {
  newTabIndex = 0
  menuTableDataList = []    //菜单对应的所有接口 数据，索引和菜单索引对应（因为只初始化一次，是静态数据）
  state = {
    showEventLog: false,
    tabList: [],
    isTempTab: true,
    activeTabKey: null,

    menuCollapse: false,
    menuSelectItemList: null,  // 菜单 选项展示数据
    currentSelectedMenuIndex:0      // 当前 是哪个菜单的索引
  }

//——————————————————————————————————————————————————————————————————————— 初始化 —————————————————————————————————————————————————————————//
  //渲染后初始化菜单数据到 menuSelectItemList
  componentDidMount(): void {
    console.log("[Main] //START//==========================================初始化")
    this.props.dispatch({
      type: 'main/getSwaggerResourceList',
      payload:0,
      callback: (returnApiDocList) => {
        console.log("[Main] 初始化获得的returnApiDocList：",returnApiDocList)
        const menuSelectItemList = this.convertApiDocListToMenuSelectItemList(returnApiDocList)
        this.setState({ menuSelectItemList })
      }
    })
    console.log("[Main] //END////==========================================初始化")
  }
  
//——————————————————————————————————————————————————————————————————————— 菜单处理 —————————————————————————————————————————————————————————//
  onMenuSelect = ({ item, key, keyPath, selectedKeys, domEvent }) => {
    this.props.dispatch({
      type: 'main/handleMenuSelect',
      payload: {
        menuData: this.menuTableDataList[this.state.currentSelectedMenuIndex],
        tabList: this.state.tabList,
        isTempTab: this.state.isTempTab,
        renderTabItem: this.renderTabItem,
        key,
        renderTabTitle: this.renderTabTitle
      },
      callback: (newTabList, activeTabKey) => {
        this.setState({
          tabList: newTabList,
          activeTabKey,
        });
      }

    })
  }
      //选择框选择时，刷新菜单数据
      onSelectRefreshMenuData=LabeledValue=>{
        this.setState({
          currentSelectedMenuIndex:LabeledValue
        })
        // const currentSwaggerResource=this.props.main.swaggerResourceList[LabeledValue];
        // this.props.dispatch({
        //   type: 'main/getApiDocs',
        //   payload:currentSwaggerResource?.location,
        //   callback: (apiInfoMap) => {
        //     const menuShowData = this.convertApiDocListToMenuSelectItemList(apiInfoMap)
        //     this.setState({ menuShowData })
        //   }
        // })
      }
  
//——————————————————————————————————————————————————————————————————————— tab栏处理 —————————————————————————————————————————————————————————//

  renderTabTitle = (currentMenuData) => {
    return <div>{currentMenuData.getKey} {currentMenuData.getObj["summary"]}</div>
  }


  renderTabItem = ({ getObj, getKey, pathObj, pathKey, definitionMap, tagObj }) => {
    const parameters = getObj?.parameters;
    let colorStyle
    switch (getKey.toUpperCase()) {
      case "GET": colorStyle = styles.getColorHeader; break;
      case "POST": colorStyle = styles.postColorHeader; break;
      case "PUT": colorStyle = styles.putColorHeader; break;
      case "DELETE": colorStyle = styles.deleteColorHeader; break;
    }
    const {resultTableList,resultExamplePostJson}=this.renderTableData(parameters,definitionMap)
    return <div className={styles.contentParentBox}>
      {/* 路径 */}
      <div className={styles.contentHeader}>
        <span className={styles.contentHeaderGray}>{tagObj?.name ? tagObj?.name + " / " : null}</span>
        <span className={styles.contentHeaderBold}>{getObj?.summary}</span>
      </div>


     <div className={styles.contentMiddleParent}>
         <div className={colorStyle}>
             <div className={styles.headerButton}>{getKey.toUpperCase()}</div>
             <span>{pathKey}</span>
           </div>
           {/* 请求类型 */}
           <div className={styles.requestInfoBox}><span className={styles.requestInfoLeft}>请求数据类型<i>{getObj?.consumes?getObj?.consumes.join(" "):null} </i></span>
             <span className={styles.requestInfoRight}>响应数据类型<i>{getObj?.produces?getObj?.produces.join(" "):null}</i></span>
           </div>
           {/* 请求参数 */}
           <div  className={styles.contentTitle}>请求参数</div>
           {getKey=="post"?     
           <JSONInput
                       id= 'a_unique_id'
                       placeholder = { resultExamplePostJson }
                       locale= { locale }
                       height= 'auto'
                       width="100%"
                       reset={true}
                       onKeyPressUpdate={false}
                       confirmGood={false}/>
                       :null}
              
             {/* //START//================================================== 遍历请求参数(get post 都是parameters) */}
                <Table
                  columns={columns}
                  pagination={false}
                  // rowSelection={{ ...rowSelection, checkStrictly }}
                  dataSource={resultTableList}
                />
             {/* //END  //================================================== 遍历请求参数 */}
           {/* 请求参数 */}
           <div  className={styles.contentTitle}>响应参数</div>
     </div>

    </div>
  }

  onChangePane = activeTabKey => {
    this.setState({ activeTabKey });
  }
  onTabEdit = (targetKey, action) => {
    this[action + "Tab"](targetKey);
  };
  addTab = () => {
    const { tabList } = this.state;
    const activeTabKey = `newTab${this.newTabIndex++}`;
    const newPanes = [...tabList];
    newPanes.push({ title: 'New Tab', content: 'Content of new Tab', key: activeTabKey });
    this.setState({
      tabList: newPanes,
      activeTabKey,
    });
  };

  removeTab = targetKey => {
    const { tabList, activeTabKey } = this.state;
    let newActiveKey = activeTabKey;
    let lastIndex;
    tabList.forEach((pane, i) => {
      if (pane.key === targetKey) {
        lastIndex = i - 1;
      }
    });
    const newPanes = tabList.filter(pane => pane.key !== targetKey);
    if (newPanes.length && newActiveKey === targetKey) {
      if (lastIndex >= 0) {
        newActiveKey = newPanes[lastIndex].key;
      } else {
        newActiveKey = newPanes[0].key;
      }
    }
    this.setState({
      tabList: newPanes,
      activeTabKey: newActiveKey,
    });
  };

//——————————————————————————————————————————————————————————————————————— 事件处理 —————————————————————————————————————————————————————————//


  deepModelObj =  (definitionMap,currentDefinitionKey)=>{
    let currentDefinitionValue = definitionMap[currentDefinitionKey]
    //找不到其他实体，跳出
    if(!currentDefinitionValue){
      return;
    }
    let resultList=[]
    let resultPostJson={}

    lodash.forEach(currentDefinitionValue?.properties, (propertyObj, propertyKey) => {
      let isRequired = lodash.includes(currentDefinitionValue?.required, propertyKey) //例如： 判断password是否必须
      let childrenModelDefinitionKey =propertyObj?.items?.$ref.replace(/#.*\//g, "")
      let currentData={
        name:propertyKey,
        description:propertyObj?.description,
        requestType:null,  //实体内字段都设置成null
        required:isRequired,
        dataType:propertyObj?.type,
        schema:childrenModelDefinitionKey,
        key:Math.random()*100000+propertyKey+propertyObj?.description,
        children:null
      }

      //包含其他实体
      if(childrenModelDefinitionKey){
        const {resultList:resultListDeep,resultPostJson:resultPostJsonDeep}=this.deepModelObj(definitionMap,childrenModelDefinitionKey)
        currentData={
          ...currentData,
          children:resultListDeep
        }
        resultPostJson={
          ...resultPostJson,
          [propertyKey]:propertyObj?.type=="array"?[resultPostJsonDeep]:resultPostJsonDeep
        }
      }else{
        resultPostJson={
          ...resultPostJson,
          [propertyKey]:propertyObj?.example
        }
      }
      resultList.push(currentData)
    })
    return {resultList,resultPostJson}

  }
  renderTableData=(parameters,definitionMap)=>{
    console.log("[Main] 执行了renderTableData")
      let resultTableList=[]
      let resultExamplePostJson={}
      lodash.forEach(parameters, (parameterObj, parameterInd) => {
             let currentDefinitionKey = parameterObj?.schema?.$ref.replace(/#.*\//g, "") // 例如：用户表实体
             let currentObj={
               name:parameterObj?.name,
               description:parameterObj?.description,
               requestType:parameterObj?.in,
               required:parameterObj?.required,
               dataType:parameterObj?.type,
               schema:currentDefinitionKey,
                key:Math.random()*100000+parameterObj?.name+parameterObj?.description,
                children:null
             }
             //引用了实体
             if(currentDefinitionKey){
              const {resultList,resultPostJson} = this.deepModelObj(definitionMap,currentDefinitionKey)
              resultExamplePostJson=resultPostJson
              currentObj={
                ...currentObj,
                children:resultList
              }
             }
             resultTableList.push(currentObj)
      })
      return {resultTableList,resultExamplePostJson}
  }

//——————————————————————————————————————————————————————————————————————— 工具方法处理 —————————————————————————————————————————————————————————//
  toggleCollapseHandler = () => {
    this.setState({
      menuCollapse: !this.state.menuCollapse
    })
  }
  toggleEventLogHandler = () => {
    this.setState({
      showEventLog: !this.state.showEventLog
    })
  }
  //初始化菜单栏，确保数据都能给到，先临时存到menuData中，在选择菜单时再取出来 ==>   onMenuSelect
  // 获取到数据后 初始化为menuShowData
  convertApiDocListToMenuSelectItemList = (returnApiDocList) => {
    let menuSelectItemList =[]
    let currentMenuTableData={}
    for(let i=0;i<returnApiDocList.length;i++){
      let apiInfoMap=returnApiDocList[i];
      if (!apiInfoMap || !apiInfoMap.tags) return;
      const pathMap = apiInfoMap.paths;
      const tagList = apiInfoMap.tags;
      const definitionMap = apiInfoMap.definitions;
  
      const startTime = Date.now()
      const result = lodash.map(tagList, (tagObj, tagInd) => {
        let childrens = [];
        //start==遍历/xxxx对象
        lodash.forEach(pathMap, (pathObj, pathKey) => {
          //遍历当前get对象
          lodash.forEach(pathObj, (getObj, getKey) => {
            //拿到tags数组
            const tagsArray = getObj?.tags;
            const hasTag = lodash.findIndex(tagsArray, pathName => pathName == tagObj.name)
            if (hasTag >= 0) {
              const currentMenuId = Math.random() * 1000000 + getObj?.operationId
              currentMenuTableData = {
                ...currentMenuTableData,
                [currentMenuId]: {
                  pathObj: pathObj,
                  pathKey: pathKey,
                  getObj: getObj,
                  getKey: getKey,
                  tagObj,
                  definitionMap
                }
              }
              childrens.push({
                key: currentMenuId,
                label: getObj?.summary,
                icon: React.createElement(LaptopOutlined)
              })
              // console.log("childrens",childrens)
            }
  
          }
          )
        }
        )
        //end====遍历pathMap对象
        return {
          key: "parent" + tagInd,
          icon: React.createElement(UserOutlined),
          label: tagObj.name,
          children: childrens
        }
      }
      )
      console.log("spendTime:", (Date.now() - startTime) / 1000 + "s")
      menuSelectItemList.push(result)
      this.menuTableDataList.push(currentMenuTableData)
    }
    return menuSelectItemList;
  }
  //——————————————————————————————————————————————————————————————————————— 渲染 —————————————————————————————————————————————————————————//
  public render() {
    const { loading } = this.props;
    const {swaggerResourceList}=this.props.main
    console.log("[Main] render==>swaggerResourceList",swaggerResourceList)
    const {menuSelectItemList,currentSelectedMenuIndex}=this.state
    if(loading || !menuSelectItemList || !menuSelectItemList[currentSelectedMenuIndex] ) {
      return <Spin className={styles.spinCenter} /> 
    }
    return (
      <Layout style={{ height: "100%" }}>
        {/* START================================================================================= 左侧菜单 */}
        <Sider trigger={<div className={styles.leftMenuBottom} onClick={this.toggleCollapseHandler}>
          {this.state.menuCollapse ? null : <span>Saidake Api v1.0.0</span>}
          <span className={styles.collapseButton} >
            {this.state.menuCollapse ? <DoubleRightOutlined /> : <DoubleLeftOutlined />}
          </span>
        </div>}
          collapsible collapsed={this.state.menuCollapse} collapsedWidth={50} width={250}>
          {/* <div className={styles.headerSelect} onClick={this.toggleCollapseHandler}> */}
          <div className={styles.headerSelect} >
            {!this.state.menuCollapse ? <Select defaultValue={0} style={{ width: 250 }} onSelect={this.onSelectRefreshMenuData} >
              {
                lodash.map(swaggerResourceList,(val,ind)=> <Option value={ind} key={ind}>{val?.name}</Option>)
              }
            </Select> : <img className={styles.logoSmile} src={logo} />}
          </div>

            <Menu
              mode="inline"
              defaultSelectedKeys={['1']}
              defaultOpenKeys={['sub1']}
              className={styles.menuLeft}
              items={menuSelectItemList[currentSelectedMenuIndex]}
              onSelect={this.onMenuSelect}
            />
        </Sider>
        {/* END  ================================================================================= 左侧菜单 */}
        <Layout style={{ position: "relative" }}>
          {/* START================================================================================= 右侧内容 */}
          <Tabs
            onChange={this.onChangePane}
            // activeTabKey={this.state.activeTabKey}
            type="editable-card"
            hideAdd
            onEdit={this.onTabEdit}
          >
            {this.state.tabList.map(pane => (
              <TabPane tab={pane.title} key={pane.key} >
                <Layout style={{ height: "100%" }}>
                  {/* <Header className={styles.contentHeader} >{pane.title}</Header> */}
                  <Content className={styles.contentBox}>
                    {pane.content}
                    {/* 事件盒子 */}
                    <div style={{ width: this.state.showEventLog ? "325px" : 0 }} className={styles.eventLogBox} >
                      <div className={styles.eventLogButton} onClick={this.toggleEventLogHandler}>LogItem</div>
                      EventBox
                    </div>
                  </Content>
                  <Footer className={styles.footerBox} style={{ textAlign: 'center' }}>Copyright ©2022 Created by saidake</Footer>
                </Layout>
              </TabPane>
            ))}
          </Tabs>
          {/* END  ================================================================================= 右侧内容 */}
        </Layout>
      </Layout>
    )
  }
}
//——————————————————————————————————————————————————————————————————————— export —————————————————————————————————————————————————————————//

export default connect(
  ({ main, loading }) => ({
    main,
    loading: loading.models.main,
  })
)(IndexPage);
