#!/bin/sh


#//START//========================================================== 脚本使用方法
# ./deploy-server.sh start     启动（首次需要手动执行）
# ./deploy-server.sh restart   重启
# ./deploy-server.sh stop      停止
# ./deploy-server.sh log       打印日志
# ./deploy-server.sh info      打印信息
# ./deploy-server.sh status    状态
# ./deploy-server.sh degug     编译测试
#//END  //========================================================== 脚本使用方法

#//START//========================================================== 默认配置项（客户端参数会覆盖）

#脚本所在的目录
#APP_HOME=/data/loopo/back-end
APP_HOME=


#运行端口
# APP_PORT=82
APP_PORT=


#巡行环境
# APP_ACTIVE=dev
APP_ACTIVE=

#jar名称，编译时替换
#APP_JAR_NAME=match.jar
APP_JAR_NAME=



#日志位置（默认为/tmp/xxappName.log）
#JAR_LOG_PATH=/tmp/match.log
JAR_LOG_PATH=

#//END  //========================================================== 默认配置项（客户端参数会覆盖）



#//START//========================================================== 客户端参数处理
if [ "$2" != ""  ]; then
	APP_HOME=$2
fi
if [ "$3" != ""  ]; then
	APP_ACTIVE=$3
fi
if [ "$4" != ""  ]; then
	APP_JAR_NAME=$4
fi
if [ "$5" != "" ]; then
	APP_PORT=$5
fi
if [  "$6" != "" ]; then
	JAR_LOG_PATH=$6
fi

if [ "$2" == "" ] || [ "$3" == "" ] || [ "$4" == "" ] || [ "$5" == "" ] || [ "$6" == "" ]; then
	echo "missing parameter"
	exit
fi

#//END  //========================================================== 客户端参数处理

#JDK所在路径
# JAVA_HOME=/usr/local/jdk1.8.0_131  #注释则使用系统环境变量


#class名称，编译时替换
APP_CLASS=education.BootApplication

#获取当前脚本所在绝对路径
CUR_DIR=$(cd "$(dirname $0)"; pwd)


#资源配置路径
RES_PATH=$APP_HOME



#CLASS_PATH=$APP_HOME/../conf
JAR_LIB=$APP_HOME/

echo "APP_HOME: 	$APP_HOME 
APP_PORT:       $APP_PORT 
APP_ACTIVE:     $APP_ACTIVE 
APP_JAR_NAME:   $APP_JAR_NAME 
JAR_LOG_PATH:   $JAR_LOG_PATH 

JAVA_HOME:      $JAVA_HOME 
CUR_DIR:        $CUR_DIR"

#以下start2参数
#执行程序启动所使用的系统用户，考虑到安全，推荐不使用root帐号
RUNNING_USER=root

#拼凑完整的classpath参数，包括指定lib目录下所有的jar
CLASSPATH=$APP_HOME
for i in "$JAR_LIB"/*.jar; do
   CLASSPATH="$CLASSPATH":"$i"
done
#以上start2参数

#java虚拟机启动参数
#JAVA_OPTS="-ms512m -mx512m -Xmn256m -Djava.awt.headless=true -XX:MaxPermSize=128m"
JAVA_OPTS="-Xms512m -Xmx512m -Xmn256m  \
-Djava.awt.headless=true  \
-XX:MaxPermSize=128m \
-Duser.timezone=GMT+8"
#JAVA_OPTS="-Xms256m -Xmx256m -XX:PermSize=128m -XX:MaxPermSize=256m -XX:NewSize=192m -XX:MaxNewSize=384m"
JAVA_OPTS_JMX="-Xms512m -Xmx512m -Xmn256m \
-Djava.awt.headless=true \
-XX:MaxPermSize=128m \
-Dcom.sun.management.jmxremote=true \
-Dcom.sun.management.jmxremote.port=1099 \
-Dcom.sun.management.jmxremote.authenticate=false \
-Dcom.sun.management.jmxremote.ssl=false"
#调试参数
debug_port=$2
DEBUG_OPTS="-Xdebug -Xrunjdwp:transport=dt_socket,address=$debug_port,server=y,suspend=n"



# echo "JAVA_OPTS: $JAVA_OPTS"
# echo "JAVA_OPTS_JMX: $JAVA_OPTS_JMX"

#############################################
#(函数)判断程序是否已启动
#说明：
#使用JDK自带的JPS命令及grep命令组合，准确查找pid
#jps 加 l 参数，表示显示java的完整包路径
#使用awk，分割出pid ($1部分)，及Java程序名称($2部分)
#############################################
#初始化psid变量（全局）
psid=0
checkpid() {
#javaps=`ps ax|grep java|grep $APP_JAR_NAME |grep -v grep`
javaps=`$JAVA_HOME/bin/jps -l|grep $APP_JAR_NAME`
#javaps=`ps ax|grep java|grep $APP_HOME|grep $APP_CLASS`
if [ -n "$javaps" ]; then
psid=`echo $javaps | awk '{print $1}'`
else
psid=0
fi
}


###################################
#(函数)启动程序
#
#说明：
#1. 首先调用checkpid函数，刷新$psid全局变量
#2. 如果程序已经启动（$psid不等于0），则提示程序已启动
#3. 如果程序没有被启动，则执行启动命令行
#4. 启动命令执行后，再次调用checkpid函数
#5. 如果步骤4的结果能够确认程序的pid,则打印[OK]，否则打印[Failed]
#注意：echo -n 表示打印字符后，不换行
#注意: "nohup 某命令 &gt;/dev/null 2&gt;&amp;1 &amp;" 的用法
###################################
start2() {
echo "******************************************************"
checkpid
if [ $psid -ne 0 ]; then
echo "======================================================"
echo "program: $APP_CLASS has started! (进程号=$psid)"
echo "======================================================"
else
echo "start $APP_CLASS ..."
nohup java $JAVA_OPTS -jar $JAR_LIB/$APP_JAR_NAME >> nohup.out 2>&1 &
checkpid
if [ $psid -ne 0 ]; then
echo "Program: $APP_CLASS started successfully! (PID=$psid)"
else
echo "Program: $APP_CLASS startup failed!"
fi
fi
echo "******************************************************"
}


###################################
#(函数)启动程序
#
#说明：
#1. 首先调用checkpid函数，刷新$psid全局变量
#2. 如果程序已经启动（$psid不等于0），则提示程序已启动
#3. 如果程序没有被启动，则执行启动命令行
#4. 启动命令执行后，再次调用checkpid函数
#5. 如果步骤4的结果能够确认程序的pid,则打印[OK]，否则打印[Failed]
#注意：echo -n 表示打印字符后，不换行
#注意: "nohup 某命令 &gt;/dev/null 2&gt;&amp;1 &amp;" 的用法
###################################




start() {
echo "*****************************************************"
checkpid
if [ $psid -ne 0 ]; then
echo "====================================================="
echo "Program: $APP_CLASS has started! (PID=$psid)"
echo "====================================================="
else
echo "start $APP_CLASS ..."
#nohup java $JAVA_OPTS -jar $JAR_LIB/$APP_JAR_NAME >> nohup.out 2>&1 &
#JAVA_CMD="nohup java $JAVA_OPTS -classpath $CLASSPATH $APP_CLASS >/dev/null 2>&1 &"
#su - $RUNNING_USER -c "$JAVA_CMD"
# nohup java $JAVA_OPTS -jar $JAR_LIB/$APP_JAR_NAME --spring.profiles.active=$APP_ACTIVE --server.port=$APP_PORT >> /dev/null 2>&1 &
nohup java $JAVA_OPTS -jar $JAR_LIB/$APP_JAR_NAME --spring.profiles.active=$APP_ACTIVE  --server.port=$APP_PORT  >> /dev/null 2>&1 &
checkpid
if [ $psid -ne 0 ]; then
echo "Program: $APP_CLASS started successfully! (PID=$psid)"
else
echo "Program: $APP_CLASS startup failed!"
fi
fi
echo "****************************************************"
}


###################################
#(函数)启动程序 (加监控,开启JMX端口)
#
#说明：
#1. 首先调用checkpid函数，刷新$psid全局变量
#2. 如果程序已经启动（$psid不等于0），则提示程序已启动
#3. 如果程序没有被启动，则执行启动命令行
#4. 启动命令执行后，再次调用checkpid函数
#5. 如果步骤4的结果能够确认程序的pid,则打印[OK]，否则打印[Failed]
#注意：echo -n 表示打印字符后，不换行
#注意: "nohup 某命令 &gt;/dev/null 2&gt;&amp;1 &amp;" 的用法
###################################


startjmx() {
echo "****************************"
checkpid
if [ $psid -ne 0 ]; then
echo "================================"
echo "程序: $APP_CLASS 已经启动! (进程号=$psid)"
echo "================================"
else
echo "启动 $APP_CLASS ..."
nohup java $JAVA_OPTS $JAVA_OPTS_JMX -jar $JAR_LIB/$APP_JAR_NAME >> nohup.out 2>&1 &
checkpid
if [ $psid -ne 0 ]; then
echo "程序: $APP_CLASS 启动成功! (进程号=$psid) [OK]"
else
echo "程序: $APP_CLASS 启动失败! [Failed]"
fi
fi
echo "****************************"
}
###################################
#(函数)debug启动程序
#
#说明：
#1. 首先调用checkpid函数，刷新$psid全局变量
#2. 如果程序已经启动（$psid不等于0），则提示程序已启动
#3. 如果程序没有被启动，则执行启动命令行
#4. 启动命令执行后，再次调用checkpid函数
#5. 如果步骤4的结果能够确认程序的pid,则打印[OK]，否则打印[Failed]
#注意：echo -n 表示打印字符后，不换行
#注意: "nohup 某命令 &gt;/dev/null 2&gt;&amp;1 &amp;" 的用法
###################################


debug() {
echo "****************************"
if [ ! -n "$debug_port" ]; then
echo "=======未输入调试端口========="
echo "[Failed]"
return
fi
checkpid
if [ $psid -ne 0 ]; then
echo "================================"
echo "程序: $APP_CLASS 已经启动! (进程号=$psid)"
echo "================================"
else
echo "调试启动 $APP_CLASS ..."
nohup java $JAVA_OPTS -Djava.ext.dirs=$JAR_LIB $APP_JAR_NAME >> nohup.out 2>&1 &
checkpid
if [ $psid -ne 0 ]; then
echo "调试程序: $APP_CLASS 启动成功! (进程号=$psid,调试端口=$debug_port) [OK]"
else
echo "程序: $APP_CLASS 启动失败! [Failed]"
fi
fi
echo "****************************"
}
###################################
#(函数)停止程序
#
#说明：
#1. 首先调用checkpid函数，刷新$psid全局变量
#2. 如果程序已经启动（$psid不等于0），则开始执行停止，否则，提示程序未运行
#3. 使用kill -2 pid命令进行强制杀死进程
#4. 执行kill命令行紧接其后，马上查看上一句命令的返回值: $?
#5. 如果步骤4的结果$?等于0,则打印[OK]，否则打印[Failed]
#6. 为了防止java程序被启动多次，这里增加反复检查进程，反复杀死的处理（递归调用stop）。
#注意：echo -n 表示打印字符后，不换行
#注意: 在shell编程中，"$?" 表示上一句命令或者一个函数的返回值
###################################


stop() {
echo "****************************"
checkpid

if [ $psid -ne 0 ]; then
echo "stoping $APP_CLASS ...(PID=$psid)"
kill -2 $psid
if [ $? -eq 0 ]; then
echo "Program: $APP_CLASS stoped successfully!(PID=$psid) "
else
echo "Program: $APP_CLASS stoped failed!(PID=$psid) "
fi


sleep 2
checkpid
if [ $psid -ne 0 ]; then
kill -9 $psid
fi
else
echo "warn: $APP_CLASS 没有运行..."
fi
echo "****************************"
}
###################################
#(函数)检查程序运行状态
#
#说明：
#1. 首先调用checkpid函数，刷新$psid全局变量
#2. 如果程序已经启动（$psid不等于0），则提示正在运行并表示出pid
#3. 否则，提示程序未运行
###################################


status() {
echo "****************************"
checkpid
if [ $psid -ne 0 ]; then
echo "$APP_CLASS 正在运行! (进程号=$psid)"
else
echo "$APP_CLASS 没有运行"
fi
echo "****************************"
}
###################################
#(函数)打印系统环境参数
###################################
info() {
echo "System Information:"
echo "****************************"
echo `head -n 1 /somnus/issue`
echo `uname -a`
echo
echo "JAVA_HOME=$JAVA_HOME"
echo `java -version`
echo
echo "APP_HOME=$APP_HOME"
echo "APP_JAR=$APP_CLASS"
echo "****************************"
}
###################################
#(函数)打印log
###################################
log() {
tail -n 500 $JAR_LOG_PATH
}
###################################
#读取脚本的第一个参数($1)，进行判断
#参数取值范围：{start|debug|stop|restart|status|info|log}
#如参数不在指定范围之内，则打印帮助信息
###################################


case "$1" in
'start')
start
;;
'debug')
debug
;;
'stop')
stop
;;
'restart')
stop
start
;;
'status')
status
;;
'info')
info
;;
'log')
log
;;
'startjmx')
startjmx
;;
'start2')
start2
;;
*)


echo "=======参数错误========="
echo "exp ：$0 start {start|debug|stop|restart|status|info|log}"
exit 1
esac
exit 0
