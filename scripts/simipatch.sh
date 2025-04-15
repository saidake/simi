#!/bin/bash
SERVER_IP=192.168.127.128
LOGIN_USER=root
LOGIN_PWD=root

#1. Copy local files to the server
rsync -av ./assets/trust-policy.json LOGIN_USER@$SERVER_IP:~/tmp
#2. Back up the server files

#3. Verify the existence of the backup file.

#4. Overwrite the server files.
C:\Users\simi\Desktop\DevProjects\simi-sandbox\scripts
#5. Execute post-transfer commands.