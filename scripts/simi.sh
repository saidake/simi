#!/bin/sh

#==========================================================
# Usage
# ./deploy-server.sh start     - Start the application
# ./deploy-server.sh restart   - Restart the application
# ./deploy-server.sh stop      - Stop the application
# ./deploy-server.sh log       - Print log file
# ./deploy-server.sh info      - Show application info
# ./deploy-server.sh status    - Display application status
# ./deploy-server.sh debug     - Start application in debug mode
# ./deploy-server.sh startjmx  - Start application with JMX enabled
#==========================================================

#==========================================================
# Configuration
# Below are the main configuration variables used to control
# the application deployment process. These variables can be
# overwritten by passing them as arguments when calling the script.
#
# The root directory where the application is deployed
#APP_HOME=
APP_HOME=~/simi
# The port the application will listen on
#APP_PORT=
APP_PORT=80
# The active Spring profile (e.g., dev, prod, etc.)
#APP_ACTIVE=
APP_ACTIVE=dev
# The name of the JAR file containing the application
#APP_JAR_NAME=
APP_JAR_NAME=simi.jar
# The path where the application logs will be stored
#JAR_LOG_PATH=
APP_HOME=/tmp/simi.log
#==========================================================
# Optional Parameters
# These parameters can be passed as arguments to override
# the default configuration.
# Usage: ./deploy-server.sh <command> <app_home> <app_active> <app_jar_name> <app_port> <log_path>
#==========================================================

# Optional: Set JAVA_HOME if not using system environment variables
# JAVA_HOME=/usr/local/jdk1.8.0_131

# Directory where the script is located
CUR_DIR=$(cd "$(dirname "$0")"; pwd)

# User running the application (recommended: non-root)
RUNNING_USER=root

JAVA_OPTS="-Xms512m -Xmx512m -Xmn256m -Djava.awt.headless=true -XX:MaxPermSize=128m -Duser.timezone=GMT+8"

JAVA_OPTS_JMX="-Xms512m -Xmx512m -Xmn256m -Djava.awt.headless=true -XX:MaxPermSize=128m -Dcom.sun.management.jmxremote=true \
-Dcom.sun.management.jmxremote.port=1099 -Dcom.sun.management.jmxremote.authenticate=false -Dcom.sun.management.jmxremote.ssl=false"

# Debugging options (replace port as needed)
DEBUG_OPTS="-Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=n,address="

#==========================================================
# Handle parameters
#==========================================================

if [ "$2" != "" ]; then APP_HOME=$2; fi
if [ "$3" != "" ]; then APP_ACTIVE=$3; fi
if [ "$4" != "" ]; then APP_JAR_NAME=$4; fi
if [ "$5" != "" ]; then APP_PORT=$5; fi
if [ "$6" != "" ]; then JAR_LOG_PATH=$6; fi

# Check for missing required parameters
if [ -z "$APP_HOME" ] || [ -z "$APP_ACTIVE" ] || [ -z "$APP_JAR_NAME" ] || [ -z "$APP_PORT" ] || [ -z "$JAR_LOG_PATH" ]; then
    echo "Missing required parameters."
    exit 1
fi

#==========================================================
# Helper Functions
#==========================================================

checkpid() {
    psid=$(pgrep -f $APP_JAR_NAME)
    psid=${psid:-0}
}


# Start the application
start() {
    echo "*****************************************************"
    checkpid
    if [ $psid -ne 0 ]; then
        echo "Program $APP_JAR_NAME is already running! (PID=$psid)"
    else
        echo "Starting $APP_JAR_NAME..."
        nohup java $JAVA_OPTS -jar $APP_HOME/$APP_JAR_NAME --spring.profiles.active=$APP_ACTIVE --server.port=$APP_PORT >> $JAR_LOG_PATH 2>&1 &
        checkpid
        if [ $psid -ne 0 ]; then
            echo "$APP_JAR_NAME started successfully! (PID=$psid)"
        else
            echo "$APP_JAR_NAME failed to start!"
        fi
    fi
    echo "*****************************************************"
}

# Start the application with JMX enabled
startjmx() {
    echo "*****************************************************"
    checkpid
    if [ $psid -ne 0 ]; then
        echo "Program $APP_JAR_NAME is already running! (PID=$psid)"
    else
        echo "Starting $APP_JAR_NAME with JMX enabled..."
        nohup java $JAVA_OPTS_JMX -jar $APP_HOME/$APP_JAR_NAME >> $JAR_LOG_PATH 2>&1 &
        checkpid
        if [ $psid -ne 0 ]; then
            echo "$APP_JAR_NAME started with JMX! (PID=$psid)"
        else
            echo "$APP_JAR_NAME failed to start with JMX!"
        fi
    fi
    echo "*****************************************************"
}

# Start the application in debug mode
# shellcheck disable=SC2120
debug() {
    debug_port=$2
    if [ -z "$debug_port" ]; then
        echo "Debug port not provided."
        return
    fi
    echo "*****************************************************"
    checkpid
    if [ $psid -ne 0 ]; then
        echo "Program $APP_JAR_NAME is already running! (PID=$psid)"
    else
        echo "Starting $APP_JAR_NAME in debug mode (Port: $debug_port)..."
        nohup java $DEBUG_OPTS$debug_port -jar $APP_HOME/$APP_JAR_NAME >> $JAR_LOG_PATH 2>&1 &
        checkpid
        if [ $psid -ne 0 ]; then
            echo "$APP_JAR_NAME started in debug mode! (PID=$psid, Debug port=$debug_port)"
        else
            echo "$APP_JAR_NAME failed to start in debug mode!"
        fi
    fi
    echo "*****************************************************"
}

# Stop the application
stop() {
    echo "*****************************************************"
    checkpid
    if [ $psid -ne 0 ]; then
        echo "Stopping $APP_JAR_NAME (PID=$psid)..."
        kill -2 $psid
        if [ $? -eq 0 ]; then
            echo "$APP_JAR_NAME stopped successfully!"
        else
            echo "Failed to stop $APP_JAR_NAME."
        fi
    else
        echo "$APP_JAR_NAME is not running."
    fi
    echo "*****************************************************"
}

# Restart the application
restart() {
    stop
    sleep 1
    start
}

# Display the current status of the application
status() {
    checkpid
    if [ $psid -ne 0 ]; then
        echo "$APP_JAR_NAME is running (PID=$psid)."
    else
        echo "$APP_JAR_NAME is not running."
    fi
}

# Tail the log file
log() {
    tail -f $JAR_LOG_PATH
}

# Display application information
info() {
    echo "*****************************************************"
    echo "App Home:     $APP_HOME"
    echo "App Port:     $APP_PORT"
    echo "App Profile:  $APP_ACTIVE"
    echo "App JAR:      $APP_JAR_NAME"
    echo "Log Path:     $JAR_LOG_PATH"
    echo "*****************************************************"
}

#==========================================================
# Script Execution
#==========================================================

case $1 in
    'start') start ;;
    'stop') stop ;;
    'restart') restart ;;
    'status') status ;;
    'log') log ;;
    'info') info ;;
    'startjmx') startjmx ;;
    'debug') debug ;;
    *)
        echo "Usage: $0 {start|stop|restart|status|log|info|startjmx|debug <port>}"
        exit 1
        ;;
esac
