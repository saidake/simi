@echo off
@chcp 65001 1>nul
::@chcp 936 1>nul
setlocal enabledelayedexpansion
:: jar remote windows simple deployment (transfer sdk-deploy-server.sh to the server for execution)
:: ssh-copy-id -i ~/.ssh/id_rsa.pub root@192.168.6.221      Password free authentication, GIT bash operation (manually change to the server IP that needs to upload the key)
:://START//============================================================================================================================= Script usage
::   sdk-deploy-client.bat test   init      Initialization and deployment of test server (SH script will be transferred to the server)
::   sdk-deploy-client.bat test   stop      The test service stops running (by default, the project is not packaged and the file is not transferred)
::   sdk-deploy-client.bat test   restart   Test server restart deployment
::   sdk-deploy-client.bat prod init            Production service initialization deployment
::   sdk-deploy-client.bat prod stop            The production service stops running (the default is not to pack items)
::   sdk-deploy-client.bat prod restart         Restart production service deployment

::   sdk-deploy-client.bat test  [init|restart|stop] nopack   If it is not packaged, the file will be transferred, and then the command will be executed on the server
::   sdk-deploy-client.bat test  [init|restart|stop] ignore   Do not package, do not transfer files, and only execute commands on the server
::   sdk-deploy-client.bat test  log              show log message
:://END//=============================================================================================================================== Script usage

:://START//============================================================================================================================= bluetooth-app env
set PROJECT_ROOT="D:\Desktop\DevProject\bluetooth-app"
:: Script path for remote execution (under the current directory by default)
set SERVER_BASH_PATH="D:\Desktop\DevProject\saidake-manage-project\sdk-deploy-server.sh"
:: Name of script executed remotely (copy to remote home path)
set SERVER_BASH_NAME=sdk-deploy-server.sh
:: Local jar packaging location, copy this path to the server
:: set CLIENT_MVN_JAR_PATH=target/match-1.0.jar
set CLIENT_MVN_JAR_PATH="D:\Desktop\DevProject\bluetooth-app\target\bluetooth-app-1.1-SNAPSHOT.jar"
set APP_JAR_NAME=bluetooth-app-1.1-SNAPSHOT.jar

:: Test environment  connection parameters
set TEST_HOST=101.43.3.227
set TEST_ACCOUNT=root
:: Test environment core bash parameters
set TEST_HOME=/root/bluetooth
set TEST_APP_PORT=48123
set TEST_APP_ACTIVE=prod
::set APP_JAR_NAME=bluetooth-app-1.1-SNAPSHOT.jar
set TEST_JAR_LOG_PATH=%TEST_HOME%/logs/1.1/sdklog-bluetooth-app-%Date:~10,4%-%Date:~4,2%-%Date:~7,2%.0.log

:: prod env
:: set PROD_HOME=/home/loopo/back-end
:: set PROD_HOST=106.75.226.3
:: set PROD_ACCOUNT=root
:: prod env sdk-deploy-server parameters
:: set PROD_APP_ACTIVE=prod
:: set PROD_APP_PORT=82
:: set PROD_JAR_LOG_PATH=%PROD_HOME%/logs/sdkmatch.log
:://END//=============================================================================================================================== bluetooth-app env


:://START//============================================================================================================================= Core logic
call :handleInitParameter %0 %1 %2

set showMsg=[%1 ------ %realHost%@%realAccount%:%realHome%]
:: show log message
if "%2" EQU "log" (
    echo //START//[client] show server log  %showMsg% ===================================================================
    call ssh %realAccount%@%realHost% cd %realHome%; ./%SERVER_BASH_NAME% log %realHome% %realAppActive% %APP_JAR_NAME% %realAppPort% %realJarLogPath%;
    ::   ssh execute command          cd to main home path      execute log command， pass 5 primary parameters
    echo //END  //[client] show server log  %showMsg% ===================================================================
    goto :endLine
)
::package command
echo //START//[client] package springboot  %showMsg% ===================================================================
if "%3" NEQ "ignore" if "%2" NEQ "stop" if "%3" NEQ "nopack" (
    cd %PROJECT_ROOT%
    call mvn clean package -Dmaven.test.skip=true
)
echo //END  //[client] package springboot  %showMsg% ===================================================================


echo //START//[client] copy to server      %showMsg% ===================================================================
if "%3" NEQ "ignore" (
    call :copyFileToServer %2
) else (
    echo ignore copying file
)
echo //END  //[client] copy to server      %showMsg% ===================================================================


echo //START//[server] execute springboot  %showMsg% ===================================================================
call :excuseBash %2
echo //END  //[server] execute springboot  %showMsg% ===================================================================
goto :endLine
:://END//=============================================================================================================================== Core logic


:://START//============================================================================================================================= Tool function
:: --------------------------------------------------------------  handleInitParameter(xxx.bat,qa,init)
:handleInitParameter
if "%2" == "test" (
    set realHome=%TEST_HOME%
    set realHost=%TEST_HOST%
    set realAccount=%TEST_ACCOUNT%

    set realAppActive=%TEST_APP_ACTIVE%
    set realAppPort=%TEST_APP_PORT%
    set realJarLogPath=%TEST_JAR_LOG_PATH%
) else if "%2" == "prod" (
    set realHome=%PROD_HOME%
    set realHost=%PROD_HOST%
    set realAccount=%PROD_ACCOUNT%

    set realAppActive=%PROD_APP_ACTIVE%
    set realAppPort=%PROD_APP_PORT%
    set realJarLogPath=%PROD_JAR_LOG_PATH%
)  else (
	echo.
	echo.
	echo please enter a right command like this: %1  ^<qa/prod^>
	echo example: %1 qa
	echo.
    goto endLine
)
goto :EOF

:: --------------------------------------------------------------  copyFileToServer(init)
:copyFileToServer
echo.
if "%1" == "init" (
	echo Initialize copying server bash and jar to the server^.^.^.
	call ssh %realAccount%@%realHost% mkdir %realHome% -p
	call scp -C %CLIENT_MVN_JAR_PATH% %SERVER_BASH_PATH% %realAccount%@%realHost%:%realHome%
) else if "%1" == "stop" (
	echo stoping server bash and jar^.^.^.
) else if "%1" == "restart" (
	call scp -C %CLIENT_MVN_JAR_PATH% %realAccount%@%realHost%:%realHome%
)
goto :EOF

:: -------------------------------------------------------------- excuseBash(init)
:excuseBash
echo.
if "%1" == "init" (
	echo Initialize server bash to start jar first time^.^.^.
	call ssh %realAccount%@%realHost% cd %realHome%; chmod +x %SERVER_BASH_NAME%; ./%SERVER_BASH_NAME% start %realHome% %realAppActive% %APP_JAR_NAME% %realAppPort% %realJarLogPath%;
) else if "%1" == "stop" (
	call ssh %realAccount%@%realHost% cd %realHome%; ./%SERVER_BASH_NAME% stop %realHome% %realAppActive% %APP_JAR_NAME% %realAppPort% %realJarLogPath%;
) else if "%1" == "restart" (
	call ssh %realAccount%@%realHost% cd %realHome%; ./%SERVER_BASH_NAME% restart %realHome% %realAppActive% %APP_JAR_NAME% %realAppPort% %realJarLogPath%;
)
goto :EOF
:://END//=============================================================================================================================== Tool function
:endLine
cd %~dp0
cmd /k