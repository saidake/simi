##主机 = local
    oracle：  http://127.0.0.1:1521     [ORCL] sdk = sdk      [ORCL] system = oracle
    mongo:    http://127.0.0.1:27017    [db/smp] smp  = smp   [db/admin] admin = admin
##主机 = test-empty
    安装列表/usr/local:     jdk-1.8.0_131    jdk-11.0.1
##主机 = test-main(192.168.22.133)
linux用户组 <br/>
jenkins  jenkins <br/>

安装列表/usr/local:     jdk-1.8.0_131    jdk-11.0.1     apache-maven-3.8.5 <br/>
安装列表yum： <br/>

jenkins：    http://192.168.22.133:48123     admin = admin123 <br/>
##主机 = test-openshift1(192.168.22.134)
安装列表/usr/local:     jdk-1.8.0_131    jdk-11.0.1 <br/>
安装列表yum： docker <br/>
/etc/hosts <br/>
192.168.22.134 master.openshift.com master <br/>
192.168.22.137 node1.openshift.com node1 <br/>
192.168.22.138 node2.openshift.com node2 <br/>
##主机 = test-openshift2(192.168.22.137)
##主机 = test-openshift3(192.168.22.138)