#!/bin/bash

# Import global environment variables
source ../AAAconfig.sh

# Zip files
# ssh-copy-id root@192.168.127.128   Copy public key to the remote server
invoke_function_bash_path=$ROOT'\aws\remote\invoke-function.sh'
remote_work_path='~'

# Execute bash file remotely
# shellcheck disable=SC2029
ssh "$TEST_AMAZON_USER"@"$TEST_AMAZON_IP" "cd $remote_work_path  && bash -l -c 'bash -s'" < "$invoke_function_bash_path"

# shellcheck disable=SC2029
ssh "$TEST_AMAZON_USER"@"$TEST_AMAZON_IP" "cd $remote_work_path  && echo 'print output.json: ' && cat output.json"
