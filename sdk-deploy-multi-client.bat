@echo off
@chcp 65001
::@chcp 936 1>nul
::@chcp 936 1>nul
setlocal enabledelayedexpansion
:: Jar remote windows easy deployment
:: Transfer SDK deploy server to the server for execution
:: author:  saidake
:: email:	saidake@qq.com
:://START//============================================================================================================================= Script usage
::   sdk-deploy-client.bat test init      		  	test server 	initialization deployment 
::   sdk-deploy-client.bat test init ignore		  	test server   	initialization deployment and don't transfer jar files
::   sdk-deploy-client.bat test init notranser		test server   	initialization deployment and don't transfer jar files
::   sdk-deploy-client.bat test init nopack		  	test server   	initialization deployment 
::   sdk-deploy-client.bat test stop      			test server 	stop running
::   sdk-deploy-client.bat test           			test server 	restart

::   sdk-deploy-client.bat prod init      			prod server 	initialization deployment 
::   sdk-deploy-client.bat prod stop      			prod server 	stop running
::   sdk-deploy-client.bat prod          			prod server 	restart

::   sdk-deploy-client.bat bluetooth init      			prod server 	
:://END//=============================================================================================================================== Script usage

:://START//============================================================================================================================= windows configration
:: the name of the script executed remotely (placed in the same directory as the current script)
set PROJECT_HOME=D:\Desktop\DevProject\test-maven\aftersale-project
set SERVER_BASH_NAME=sdk-deploy-server.sh
:: public jar packaging location
set CLIENT_MVN_JAR_PATH[1]=D:\Desktop\DevProject\test-maven\aftersale-project\aftersale-gateway\target\aftersale-gateway-1.0.0.jar
set CLIENT_MVN_JAR_PATH[2]=D:\Desktop\DevProject\test-maven\aftersale-project\aftersale-services\aftersale-main\target\aftersale-main-1.0.0.jar
set CLIENT_MVN_JAR_PATH[3]=D:\Desktop\DevProject\test-maven\aftersale-project\aftersale-services\aftersale-system\target\aftersale-system-1.0.0.jar
:: public jar package name
set APP_JAR_NAME[1]=aftersale-gateway-1.0.0.jar
set APP_JAR_NAME[2]=aftersale-main-1.0.0.jar
set APP_JAR_NAME[3]=aftersale-system-1.0.0.jar
:: public port env
set APP_PORT[1]=8099
set APP_PORT[2]=8098
set APP_PORT[3]=8097

set JAR_COUNT=3

:: test env  --------------------------
set SERVER_HOST[1]=101.43.3.227
set SERVER_ACCOUNT[1]=root

set APP_HOME[1]=/root/aftersale
set APP_ACTIVE[1]=prod
set JAR_LOG_PATH[1]=%APP_HOME[1]%/logs/sdklog-aftersale-main-%Date:~10,4%-%Date:~4,2%-%Date:~7,2%.0.log



:: prod env --------------------------
set SERVER_HOST[2]=192.168.22.129ffff
set SERVER_ACCOUNT[2]=rootff

set APP_HOME[2]=/root/back-endff
set APP_ACTIVE[2]=devff

set JAR_LOG_PATH[2]=/root/back-end/match.logff
:://END//=============================================================================================================================== windows configration


:://START//============================================================================================================================= Core logic
call :handleInitParameter %0 %1 %2
:: main log
if "%2" EQU "log" (
    echo //START//[client] show server log  %showMsg% ===================================================================
	call ssh %realAccount%@%realHost%  %realHome%/%SERVER_BASH_NAME% log %realHome% %realAppActive% !APP_JAR_NAME[2]! !APP_PORT[2]! %realJarLogPath%;
    echo //END  //[client] show server log  %showMsg% ===================================================================
    echo.
    goto :endLine
)

set showMsg=[%1 ------ %realHost%@%realAccount%:%realHome%]

echo //START//[client] package springboot  %showMsg% ===================================================================
if "%3" EQU "nopack" (
	echo pass pack...
)else if "%3" EQU "ignore" (
	echo pass pack...
)else (
	cd %PROJECT_HOME% && call mvn clean package 
	cd %~dp0
)
echo //END  //[client] package springboot  %showMsg% ===================================================================


echo //START//[client] copy to server      %showMsg% ===================================================================
call :copyFileToServer %2 %3
echo //END  //[client] copy to server      %showMsg% ===================================================================


echo //START//[server] execute springboot  %showMsg% ===================================================================
call :excuseBash %2
echo //END  //[server] execute springboot  %showMsg% ===================================================================
goto :endLine
:://END//=============================================================================================================================== Core logic


:://START//============================================================================================================================= Tool function
:: --------------------------------------------------------------  handleInitParameter(xxx.bat,qa,init) 
:handleInitParameter

:: defines the number corresponding to the environment
if "%2" EQU "test" (set ENV_NUM=1) else if "%2" EQU "prod" ( set ENV_NUM=2) else (
	echo.
	echo.
	echo please enter a right command like this: %1  ^<test^>
	echo example: %1 test init
	echo.
    goto endLine
    exit
)

echo SERVER_HOST			!SERVER_HOST[%ENV_NUM%]!
echo SERVER_ACCOUNT			!SERVER_ACCOUNT[%ENV_NUM%]!
echo APP_HOME			!APP_HOME[%ENV_NUM%]!
echo APP_ACTIVE			!APP_ACTIVE[%ENV_NUM%]!
echo JAR_LOG_PATH			!JAR_LOG_PATH[%ENV_NUM%]!
set realHost=!SERVER_HOST[%ENV_NUM%]!
set realAccount=!SERVER_ACCOUNT[%ENV_NUM%]!
set realHome=!APP_HOME[%ENV_NUM%]!
set realAppActive=!APP_ACTIVE[%ENV_NUM%]!
set realJarLogPath=!JAR_LOG_PATH[%ENV_NUM%]!

goto :EOF

:: --------------------------------------------------------------  copyFileToServer(init) 
:copyFileToServer
echo.
if "%2" EQU "notranser" (
	echo pass copy jar files...
)else if "%2" EQU "ignore" (
	echo pass copy jar files...
)else if "%1" EQU "init" (
   	call scp -C %SERVER_BASH_NAME% %realAccount%@%realHost%:%realHome%
	for /l %%n in (1,1,%JAR_COUNT%) do (    
   		call scp -C !CLIENT_MVN_JAR_PATH[%%n]! %realAccount%@%realHost%:%realHome%
	)
) else if "%1" EQU "stop" (
	echo stoping server bash and jar^.^.^.
) else  (
	for /l %%n in (1,1,%JAR_COUNT%) do (    
   		call scp -C !CLIENT_MVN_JAR_PATH[%%n]! %realAccount%@%realHost%:%realHome%
	)
)
goto :EOF

:: --------------------------------------------------------------  excuseBash(init) 
:excuseBash
echo.
if "%1" EQU "init" (
	echo Initialize server bash to start jar first time^.^.^.
	call ssh %realAccount%@%realHost% chmod +x %realHome%/%SERVER_BASH_NAME%;

	for /l %%n in (1,1,%JAR_COUNT%) do (
		call ssh %realAccount%@%realHost%  %realHome%/%SERVER_BASH_NAME% start %realHome% %realAppActive% !APP_JAR_NAME[%%n]! !APP_PORT[%%n]! %realJarLogPath%;
	)
) else if "%1" EQU "stop" (
		for /l %%n in (1,1,%JAR_COUNT%) do (
			call ssh %realAccount%@%realHost%  %realHome%/%SERVER_BASH_NAME% stop %realHome% %realAppActive% !APP_JAR_NAME[%%n]! !APP_PORT[%%n]! %realJarLogPath%;
		)
) else (
	for /l %%n in (1,1,%JAR_COUNT%) do (    
		call ssh %realAccount%@%realHost%  %realHome%/%SERVER_BASH_NAME% restart %realHome% %realAppActive% !APP_JAR_NAME[%%n]! !APP_PORT[%%n]! %realJarLogPath%;
	)

)
goto :EOF
:://END//=============================================================================================================================== Tool function
:endLine
cmd /k