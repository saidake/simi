## 主机 = local
    oracle：  http://127.0.0.1:1521     [ORCL] sdk = sdk      [ORCL] system = oracle
    mongo:    http://127.0.0.1:27017    [db/smp] smp  = smp   [db/admin] admin = admin
## 主机 = test-empty
## 主机 = test-openshift2(192.168.22.137)
## 主机 = test-openshift3(192.168.22.138)

# <font color="cyan">plugin introduction</font>
## <font color="pink">simi-plugin (Plugin Integration)</font>
# <font color="cyan">service introduction</font>
## <font color="pink">sdk-common</font>
sdk-common-core (Core Components)
## <font color="pink">sdk-integration (Gateway Integration)</font>
simi-eureka  [Registration Center](http://localhost:48992)
simi-cloud  [Registration Center](http://localhost:48993)
## <font color="pink">sdk-service (Service Integration)</font>
simi-trade [Oracle Service](http://localhost:48120/swagger-ui.html) <br/>
simi-mysql  [Mysql Service](http://localhost:48123/swagger-ui.html) <br/>
simi-oracle [Oracle Service](http://localhost:48124/swagger-ui.html) <br/>
simi-nodb   [Nodb Service](http://localhost:48125/swagger-ui.html) <br/>



