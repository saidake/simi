#!/bin/bash

# Import global environment variables
source ../AAAconfig.sh

# Zip files
# ssh-copy-id root@192.168.127.128   Copy public key to the remote server
remote_work_path='~'

# Execute bash file remotely
# shellcheck disable=SC2029
ssh "$TEST_AMAZON_USER"@"$TEST_AMAZON_IP" "bash -l -c ' \
aws dynamodb delete-table --table-name TestPerson && \
aws dynamodb delete-table --table-name TestTeacher && \
aws dynamodb delete-table --table-name TestStudent'"