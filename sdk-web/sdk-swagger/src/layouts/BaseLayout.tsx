import React, { PureComponent, Dispatch } from 'react';
import { ConfigProvider, Layout, Modal, message, Tabs } from 'antd';
import { withRouter } from 'react-router';
import styles from './BaseLayout.less'
const { Header, Footer, Sider, Content } = Layout;
import logo from "@/assets/daboluo-logo.svg";
import security from "@/assets/security.png";
import {parsePhoneNumber,parsePhoneNumberWithError,isValidPhoneNumber} from 'libphonenumber-js'


export default class BasicLayout extends PureComponent<any, any> {
    componentDidMount(): void {
        console.dir(parsePhoneNumberWithError('+8612374166484'))
        console.dir(isValidPhoneNumber('+8612374166484'))
    }
    public render(): React.ReactNode {

    const { children} = this.props;
        return <Layout style={{ minHeight: '100vh',maxHeight:"100vh", overflow:"hidden",fontWeight:"400",fontFamily:"Metropolis,Avenir Next,Helvetica Neue,Arial,sans-serif"}}>
      <Header  style={{ color: '#fff',backgroundColor:"#00364d",overflow:"hidden",paddingLeft:"30px",height:"60px",lineHeight:"60px"}}>
          <div style={{float:"left",width:"200px"}}>
              <img style={{float:"left",width:"40px",height:"60px",lineHeight:"60px"}} src={logo} alt="daboluo" />
              <div style={{float:"left",paddingLeft:"10px"}}>个人测试小站</div>
          </div>
      </Header>
      {children}
      <Footer style={{ color: '#fff',backgroundColor:"#00364d",padding:"0"}}>
        <a style={{textAlign:"center",display:"block",height:"60px",lineHeight:"60px"}}  href="https://beian.miit.gov.cn/">
            <img src={security} alt="" />
            <span>鄂ICP备2022007820号-1</span>
        </a>
      </Footer>
    </Layout>
    }
}
