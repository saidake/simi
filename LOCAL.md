# Hosts
<table>
<thead>
<tr>
    <th>Host Name</th>
    <th>Type</th>
    <th>IP address</th>
    <th>Description</th>
</tr>
</thead>
<tbody>
<tr>
    <td>test-amazon</td>
    <td>Vmware</td>
    <td>192.168.127.128</td>
    <td>Test LocalStack Server for debugging Amazon Services</td>
</tr>
</tbody>
</table>

# Local

    oracle：  http://127.0.0.1:1521     [ORCL] sdk = sdk      [ORCL] system = oracle
    mongo:    http://127.0.0.1:27017    [db/smp] smp  = smp   [db/admin] admin = admin

# Service Introduction
## simi-common
sdk-common-core (Core Components)
## simi-gateway (Gateway Integration)
simi-eureka  [Registration Center](http://localhost:48992)<br/>
simi-cloud  [Registration Center](http://localhost:48993)
## simi-service (Service Integration)
simi-trading [Oracle Service](http://localhost:48120/swagger-ui.html) <br/>
## simi-test-gradle (Test Projects with maven)
simi-mysql  [Mysql Service](http://localhost:48123/swagger-ui.html) <br/>
simi-oracle [Oracle Service](http://localhost:48124/swagger-ui.html) <br/>
simi-nodb   [Nodb Service](http://localhost:48125/swagger-ui.html) <br/>



