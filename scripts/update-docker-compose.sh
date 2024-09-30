#!/bin/bash

# Import global environment variables
source AAAconfig.sh

# Zip files
# ssh-copy-id root@192.168.127.128   Copy public key to the remote server
docker_compose_path='./assets/docker-compose.yml'
remote_work_path='~'

# Upload zip file
scp $docker_compose_path  "$TEST_AMAZON_USER"@"$TEST_AMAZON_IP":$remote_work_path
ssh "$TEST_AMAZON_USER"@"$TEST_AMAZON_IP" "docker-compose restart"