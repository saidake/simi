# Hosts
<table>
<thead>
<tr>
    <th>Host Name</th>
    <th>Type</th>
    <th>IP address</th>
    <th>Description</th>
    <th>Accounts</th>
</tr>
</thead>
<tbody>
<tr>
    <td>test-amazon</td>
    <td>Vmware</td>
    <td>192.168.127.128</td>
    <td>Test LocalStack Server for debugging Amazon Services</td>
</tr>
<tr>
    <td>test-sap-hana</td>
    <td>Vmware</td>
    <td>192.168.127.129</td>
    <td>Test SAP HANA Express Database, it is a full Hana database with features but with restrictions (max memory 32GB). </td>
    <td> 
        hxeadm = Simi110120% (admin) <br/> 
        SYSTEM, XSA_ADMIN, XSA_DEV = Simi110119% (admin)
    </td>
</tr>
<tr>
    <td>Oracle23ai</td>
    <td>Local</td>
    <td>127.0.0.1</td>
    <td>Oracle23ai</td>
    <td> 
        FREE (sid) = SYS, SYSTEM, PDBADMIN (dba) = Simi110120%  <br/>
        FREE (sid) = simi (dba) = Simi110120%  
    </td>
</tr>
</tbody>
</table>

# Others

```text
mongo:    http://127.0.0.1:27017    [db/smp] smp  = smp   [db/admin] admin = admin
```

