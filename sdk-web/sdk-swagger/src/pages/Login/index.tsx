import React, { PureComponent, Dispatch } from 'react';
import { ConfigProvider, Layout, Modal, message, Tabs,Form } from 'antd';
import { Input } from 'antd';
const FormItem = Form.Item;
import {history} from 'umi';
import styles from './index.less';


import BaseLayout from '@/layouts/BaseLayout'
const { Header, Footer, Sider, Content } = Layout;
class IndexPage extends PureComponent<any, any> {
  handleLogin=()=>{
    history.push("/main")
  }
  public render() {
    return (
      <Layout>
        <Sider className={styles.sideBar}  >
          <div>
            <div className={styles.title}>个人测试小站</div>
            <input placeholder='Username' className={styles.input}/>
            <input placeholder='Password' className={styles.input}/>
            <div>Remember me</div>
            <button className={styles.login} onClick={this.handleLogin}>LOG IN</button>
            <div>More info...</div>
          </div>
        </Sider>
        <Content>Login Content</Content>
      </Layout>
    )
  }
}


export default IndexPage;