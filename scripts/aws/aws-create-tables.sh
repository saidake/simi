#!/bin/bash

# Import global environment variables
source ../AAAconfig.sh

# Zip files
# ssh-copy-id root@192.168.127.128   Copy public key to the remote server
create_tables_bash_path=$ROOT'/aws/remote/create-tables.sh'
remote_work_path='~'

# Execute bash file remotely
# shellcheck disable=SC2029
ssh "$TEST_AMAZON_USER"@"$TEST_AMAZON_IP" "cd $remote_work_path  && bash -l -c 'bash -s'" < "$create_tables_bash_path"


