#!/bin/bash

SERVER_IP=192.168.0.1
LOCAL_PATH="./build/"
LOGIN_USER=root
LOGIN_PWD=root
REMOTE_UPLOAD_PATH='/var/ui'

sshpass -p "$LOGIN_PWD" ssh "$LOGIN_USER@$SERVER_IP" "rm -rf /var/ui/*"
sshpass -p "$LOGIN_PWD" rsync -avz "$LOCAL_PATH" "$LOGIN_USER@$SERVER_IP:$REMOTE_UPLOAD_PATH"
