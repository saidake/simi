#!/bin/bash

# Import global environment variables
source ../AAAconfig.sh

# Zip files
# ssh-copy-id root@192.168.127.128   Copy public key to the remote server
bash_path=$ROOT'\aws\remote\aws-sns-remote.sh'
remote_work_path='~'

# Execute bash file remotely
ssh "$TEST_AMAZON_USER"@"$TEST_AMAZON_IP" "cd $remote_work_path  && bash -l -c 'bash -s'" < "$bash_path"
