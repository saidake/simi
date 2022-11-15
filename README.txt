========================================================================================== 服务介绍
sdk-common
    sdk-common-core     核心组件
        config          SpringContextHolder             applicationContext存储器
        domain.config   HttpRequestInterceptor
        domain.config   ServletAttributesTaskDecorator  异步父子线程servletRequest传递
        util            公共工具
sdk-integration  网关服务
    sdk-eureka          注册中心（http://localhost:48992）
sdk-service  业务集合
    sdk-citi            股票服务（http://localhost:48124/swagger） 【依赖：sdk-eureka】
    sdk-generator       生成器服务（http://localhost:48123/swagger）
    sdk-paizhi
========================================================================================== 主机 = homehost-vm-main(192.168.22.133)
linux用户组
    oracle: dba  oracle
    usr:    root
    vm:     vmshare

安装列表/usr/local:     jdk-1.8.0_131    jdk-11.0.1     apache-maven-3.8.5
安装列表yum：

jenkins：    http://192.168.22.133:48123     admin = admin123
========================================================================================== 主机 = local
oracle：     http://localhost:1521    sdk = sdk = STD   system = oracle
