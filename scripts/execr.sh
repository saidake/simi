#!/bin/bash
# ************************************************************************************
# This script execute bash file on the remote sever. The first parameter of this bash
# file is the bash file to be executed on the remote server. The work path is the user
# path by default.
# Usage:
#   1. Update the servers.sh path if needed.
#   2. Execute this bash command:  ./execr.sh <remote-bash-file-path>
#
# Since: Oct 4, 2024
# ************************************************************************************

# Import global environment variables
source ./AAAconfig/servers.sh

# Common variables
# ssh-copy-id root@192.168.127.128   Copy public key to the remote server
exe_bash_path=$1
remote_work_path='~'

# Check if the parameter is passed
if [ -z "$exe_bash_path" ]; then
    echo "Error: No bash script file provided. Please pass the script file path as an argument."
    exit 1
fi

# Execute bash file remotely
# shellcheck disable=SC2029
ssh "$TEST_AMAZON_USER"@"$TEST_AMAZON_IP" "cd $remote_work_path && bash -l -c 'bash -s'" < "$exe_bash_path"


