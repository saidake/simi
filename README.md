# <font color="cyan">plugin introduction</font>
## <font color="pink">smp-plugin (Plugin Integration)</font>
### <font color="yellow">Smp Init</font>
Init project files by the default config file: .smp/smp.yml.<br/>
Prerequisite:<br/>
Create the configuration file in the user directory: ~/.smp/smp-init.yml<br/>
<font color="gray">Tips: The user configuration directory for Windows is "C:\Users\<username>\.smp"</font><br/>

smp-init.yml example:<br/>
```yaml
project:
  - name: smp-oracle
    path: C:\\Users\\saidake\\Desktop\\DevProject\\saidake-manage-project\\smp-service\\smp-oracle   # Parent project folder
    env: UAT
    fileList:
      - write: src/main/resources/application-local.properties    # The relative path to write the file.
        read: /${project.name}/${project.env}                     # Read folder.
                                                                    # When the path starts with "/", automatically concatenate the configuration path "~/.smp"
                                                                    # Tips: The user configuration directory for Windows is "C:\Users\<username>\.smp"
                                                                    #   ${project.name}   The project name of the current project.(e.g. "smp-oracle")
                                                                    #   ${project.path}   The project path of the current project.(e.g. "C:\\Users\\saidake\\Desktop\\DevProject\\saidake-manage-project\\smp-service\\smp-oracle")
                                                                    #   ${project.env}    The ENV of the current project.(e.g. "UAT")
                                                                    #   ${smp}            The "~/.smp" configuration path.(e.g. "C:\Users\saidake\.smp")
        type: append-properties-folder                            # Read all properties files in the directory and append them to the writing property file.
        backup: current                                           # Create a backup file in the current file directory.

      - write: src/main/resources/application-test.properties     # The relative path to write the file.
        read: /${project.name}/test.properties                    # Read property file.
        type: append-properties                              
        backup: current                                           

      - write: src/main/resources/logback.xml
        read: /${project.name}/logback.xml
        type: replace-all                                         # Replace all content with the read file.

      - write: src/main/resources/logback.xml
        read: /${project.name}/logback-replace.rp
        type: replace-string                                      # Replace matched content with the RP file, Please refer to the introduction of the RP file format below.

      - write: src/main/resources/logback.xml
        read: /${project.name}/logback.txt
        type: append-string                                       # Replace string with the read file.

      - write: src/main/resources/logback.xml
        read: /${project.name}/logback.rp
        type: line-replace                                        # Replace string with the RP file, The left value is the lineNumber, The right value is the replace content.(e.g. "2$$$String tt=99;")

      - write: src/main/resources/logback.xml
        read: /${project.name}/logback.rp
        type: line-append                                         # Append string with the RP file, The left value is the lineNumber, The right value is the append content.(e.g. "2$$$String tt=99;")

      - write: src\main\java\com\saidake\common\core\util\file\SmpTestBackupUtils.java
        type: java-annotation                                     # Each line of the write file will be preceded by '//'
        backup: current                                           
        once: true                                                # Only write once, It will determine whether it is the first write based on whether the backup file exists.

      - write: src/test/resources/smp-test/pom.xml
        read: /${project.name}/xml-append.xml
        type: xml                                                 # Write xml file, Please refer to the introduction of the pom-append.xml file format below.
        backup: current

  - name: smp-common-core
    path: C:\Users\saidake\Desktop\DevProject\saidake-manage-project\smp-common\smp-common-core
    env: UAT
    fileList:
      - write: src\main\java\com\saidake\common\core\util\file\SmpFileBackupUtils.java
        type: java-annotation
        backup: current
        once: true
```

logback.rp example: <br/>
```text
<contextName>logback</contextName>$$$<contextName>logback-replace-content</contextName>
sourceValue$$$ReplaceValue
//source//abc.cert$$$${smp}/abc.cert
```
Key values are separated by '$$$'<br/>
Even in the RP file, you can still access the env properties:<br/>
<table>
    <tr>
        <td>${project.name}</td>   
        <td>The project name of the current project.(e.g. "smp-oracle")</td>
    </tr>
    <tr>
        <td>${project.path}</td>   
        <td>The project path of the current project.(e.g. "C:\\Users\\saidake\\Desktop\\DevProject\\saidake-manage-project\\smp-service\\smp-oracle")</td>
    </tr>
    <tr>
        <td>${project.env} </td>   
        <td>The ENV of the current project.(e.g. "UAT")</td>
    </tr>
    <tr>
        <td>${smp}         </td>   
        <td>The "~/.smp" configuration path.(e.g. "C:\Users\saidake\.smp")</td></tr>
    </tr>
</table>

xml-append.xml example: <br/>
```xml
<root>
    <replace xpath="/project/dependencyManagement/dependencies/dependency">    <!-- The xpath of the replace tag-->
        <ele xpath="artifactId" value="maven-compiler-plugin" append-if-not-exists="true">  <!-- Use the artifactId xpath search under dependency, and if the value is equal to 'maven-compiler-plugin', replace it.-->
            <dependency>
                <groupId>org.saidake.mmp</groupId>
                <artifactId>smp</artifactId>
                <version>${lombok.version}</version>
                <exclusions>
                    <exclusion>
                        <groupId>org.saidake.mmp</groupId>
                        <artifactId>cc</artifactId>
                    </exclusion>
                </exclusions>
            </dependency>
        </ele>
    </replace>
    <append parent-xpath="/project/dependencies">   <!-- The xpath of the parent tag of the replace tag-->
        <dependency>
            <groupId>org.saidake.mmp</groupId>
            <artifactId>smp</artifactId>
            <version>baba</version>
        </dependency>
    </append>
</root>
```

# <font color="cyan">service introduction</font>
## <font color="pink">sdk-common</font>
sdk-common-core (Core Components)
## <font color="pink">sdk-integration (Gateway Integration)</font>
sdk-eureka  [Registration Center](http://localhost:48992)
## <font color="pink">sdk-service (Service Integration)</font>
sdk-mysql  [Mysql Service](http://localhost:48123/swagger-ui.html) <br/>
sdk-oracle [Oracle Service](http://localhost:48124/swagger-ui.html) <br/>

