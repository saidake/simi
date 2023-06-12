# <span style="color:cyan">plugin introduction</span>
## <span style="color:pink">smp-plugin (Plugin Integration)</span>
### <span style="color:yellow">Smp Init</span><br/>
Init project files by the default config file: .smp/smp.yml.
Prerequisite:<br/>
Include the configuration file in the user directory: ~/.smp/smp-init.yml<br/>

smp-init.yml example:<br/>
```yaml
project:
  - name: smp-oracle
    path: C:\\Users\\saidake\\Desktop\\DevProject\\saidake-manage-project\\smp-service\\smp-oracle   # Parent project folder
    env: UAT
    fileList:
      - write: src/main/resources/application-local.properties    # The relative path to write the file.
        read: /${project.name}/${project.env}                     # Read folder.
                                                                    # When the path starts with "/", automatically concatenate the configuration path "~/. smp"
                                                                    # Tips: The user configuration directory for Windows is "C:\Users\<username>\.smp"
                                                                    #   ${project.name}   The project name of the current project. // smp-oracle
                                                                    #   ${project.path}   The project path of the current project. // smp-oracle
                                                                    #   ${project.env}    The ENV of the current project. // smp-oracle
                                                                    #   ${smp}            The "~/.smp" configuration path
        type: append-properties-folder                            # Read all properties files in the directory and append them to the write property file.
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
        read: /${project.name}/pom-append.xml
        type: pom                                                 # Write xml file, Please refer to the introduction of the pom-append.xml file format below.
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
Key values are separated by '$$$'
Even in the RP file, you can still access the env properties:<br/>
   ${project.name}   The project name of the current project. // smp-oracle<br/>
   ${project.path}   The project path of the current project. // smp-oracle<br/>
   ${project.env}    The ENV of the current project. // smp-oracle<br/>
   ${smp}            The "~/.smp" configuration path<br/>

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

# <span style="color:cyan">service introduction</span>
## <span style="color:pink">sdk-common</span>
sdk-common-core (Core Components)
## <span style="color:pink">sdk-integration (Gateway Integration)</span>
sdk-eureka  [Registration Center](http://localhost:48992)
## <span style="color:pink">sdk-service (Service Integration)</span>
sdk-mysql  [Mysql Service](http://localhost:48123/swagger-ui.html) <br/>
sdk-oracle [Oracle Service](http://localhost:48124/swagger-ui.html) <br/>

