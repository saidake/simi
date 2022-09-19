========================================================================================== 服务介绍
sdk-common
    sdk-common-core     核心组件

sdk-integration  网关服务
    sdk-eureka          注册中心（http://localhost:48992）
sdk-service  业务集合
    sdk-citi            股票服务（http://localhost:48124/swagger） 【依赖：sdk-eureka】
    sdk-generator       生成器服务（48123）
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
========================================================================================== 初始化sql（oracle）
drop table TEST_PERSON;
create table TEST_PERSON(
    PER_ID NUMBER PRIMARY KEY,
    AGE NUMBER,
    NAME VARCHAR2(30),
    GENDER NUMBER(1)
);
INSERT INTO TEST_PERSON VALUES(1,23,'david',1);
INSERT INTO TEST_PERSON VALUES(2,22,'rose',0);
INSERT INTO TEST_PERSON VALUES(3,25,'jack',1);

drop table TEST_STUDENT;
create table TEST_STUDENT(
    STU_ID NUMBER NULL,
    PER_ID NUMBER CONSTRAINT fk_per_id REFERENCES PERSON(PER_ID) NULL,
    CLASS VARCHAR2(30) NULL,
    ROLE VARCHAR2(30) NULL
);
insert into TEST_STUDENT values(3,1,'class3','monitor');
insert into TEST_STUDENT values(4,2,'class9','leader');
insert into TEST_STUDENT values(5,1,'class9','normal');