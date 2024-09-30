#!/bin/bash

# Import global environment variables
source ../AAAconfig.sh

# Zip files
# ssh-copy-id root@192.168.127.128   Copy public key to the remote server
build_path='C:\Users\simi\Desktop\DevProjects\simi\simi-test-gradle\simi-webflux\build\libs\'
zip_file_path=$build_path'simi-lambda.zip'
jar_file_path=$build_path'simi-webflux-1.0-SNAPSHOT.jar'
trust_policy_path=$ROOT'/data/trust-policy.json'
create_function_bash_path=$ROOT'/aws/remote/create-function.sh'

remote_work_path='~'

# Zip jar file
zip -j $zip_file_path $jar_file_path

# Upload zip file
scp $zip_file_path "$trust_policy_path"  "$TEST_AMAZON_USER"@"$TEST_AMAZON_IP":$remote_work_path

# Execute bash file remotely
# shellcheck disable=SC2029
ssh "$TEST_AMAZON_USER"@"$TEST_AMAZON_IP" "cd $remote_work_path  && bash -l -c 'bash -s'" < "$create_function_bash_path"