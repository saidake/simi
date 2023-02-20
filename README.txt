openshift
udeploy
appDynamic
elk
kafka
========================================================================================== 服务介绍
sdk-common
    sdk-common-core     核心组件
sdk-integration  网关集合
    sdk-eureka          注册中心（http://localhost:48992）
sdk-service      业务集合
    sdk-trade           交易服务（http://localhost:48124/swagger） 【依赖：sdk-eureka】
    sdk-generator       生成器服务（http://localhost:48123/swagger）
========================================================================================== 主机 = local
oracle：     http://127.0.0.1:1521    [ORCL] - sdk = sdk      [ORCL] - system = oracle
========================================================================================== 主机 = test-empty
安装列表/usr/local:     jdk-1.8.0_131    jdk-11.0.1
========================================================================================== 主机 = test-main(192.168.22.133)
linux用户组
    jenkins  jenkins

安装列表/usr/local:     jdk-1.8.0_131    jdk-11.0.1     apache-maven-3.8.5
安装列表yum：

jenkins：    http://192.168.22.133:48123     admin = admin123
========================================================================================== 主机 = test-openshift1(192.168.22.134)
安装列表/usr/local:     jdk-1.8.0_131    jdk-11.0.1
安装列表yum： docker
/etc/hosts
    192.168.22.134 master.openshift.com master
    192.168.22.137 node1.openshift.com node1
    192.168.22.138 node2.openshift.com node2
========================================================================================== 主机 = test-openshift2(192.168.22.137)
========================================================================================== 主机 = test-openshift3(192.168.22.138)

