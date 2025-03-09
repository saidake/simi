#!/bin/sh
# ************************************************************************************
# A bash file used to start, restart and stop local service.
#
# Prerequisites:
#   Create the service file in the default directory `./config/simictl/<service-name>.service`.
#
# Usage
#   ./simictl.sh <service-name> start     - Start a service in the local environment.
#
# Author: Craig Brown
# Date: March 6, 2025
# ************************************************************************************

while IFS='=' read -r key value; do
    #echo "Key: $key, Value: $value"
    echo $key;
    eval "$value"
done < ./config/simictl/spark.service
