import React, { PureComponent, Dispatch } from 'react';
import { ConfigProvider, Layout, Modal, message, Tabs } from 'antd';
import { withRouter } from 'react-router';
import styles from './BaseLayout.less'
const { Header, Footer, Sider, Content } = Layout;
import logo from "@/assets/daboluo-logo.svg";
export default class BasicLayout extends PureComponent<any, any> {
    public render(): React.ReactNode {

    const { children} = this.props;
        return <Layout style={{ minHeight: '100vh',maxHeight:"100vh", overflow:"hidden",fontWeight:"400",fontFamily:"Metropolis,Avenir Next,Helvetica Neue,Arial,sans-serif"}}>
      <Header  style={{ color: '#fff',backgroundColor:"#00364d",overflow:"hidden",paddingLeft:"30px",height:"60px",lineHeight:"60px"}}>
          <div style={{float:"left",width:"200px"}}>
              <img style={{float:"left",width:"40px",height:"60px",lineHeight:"60px"}} src={logo} alt="daboluo" />
              <div style={{float:"left",paddingLeft:"10px"}}>Daboluo</div>
          </div>
      </Header>
      {children}
    </Layout>
    }
}
