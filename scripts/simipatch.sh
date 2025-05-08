#!/bin/bash
# ************************************************************************************
# This script patches a local file to a remote server. The process is designed to be
# as safe and careful as possible.
# Each step will print the executed command and need user to verify the result.
#
# Use sshpass for authentication instead of SSH credentials, as large-scale systems
# often involve many server addresses. Directly using a password is the simplest
# method.
#
# Some servers may not support `rsync -avz`, consider using scp instead.
#
# Rules:
#  * Any error will terminate the script.
#  * After each step, the user must confirm that the process works as intended.
#
# Prerequisites:
#  * Modify the required configurations in the bash file with your own values.
#
# Usage:
#   ./simi-patch.sh
#        1. Upload the 'LOCAL_FILE' to the 'REMOTE_UPLOAD_FILE'
#        2. Copy the 'REMOTE_BACKUP_SOURCE_FILE' under the folder 'REMOTE_BACKUP_FOLDER'
#           on remote server
#        3. Overwrite the 'REMOTE_OVERWRITE_TARGET_FILE' with 'REMOTE_OVERWRITE_SOURCE_FILE'
#   ./simi-patch.sh recover
#        Overwrite the 'REMOTE_OVERWRITE_TARGET_FILE' with 'REMOTE_BACKUP_TARGET_FILE'
#
# Author: Craig Brown
# Date: April 16, 2025
# ************************************************************************************

# ************************************************************ Required Configurations
SERVER_IP=192.168.127.128
LOGIN_USER=root
LOGIN_PWD=root
# Upload the LOCAL_FILE to the REMOTE_UPLOAD_FILE
LOCAL_FILE='./assets/trust-policy.json'
REMOTE_UPLOAD_FILE='~/tmp/trust-policy.json.upload'

# Copy the REMOTE_BACKUP_SOURCE_FILE to the file REMOTE_BACKUP_TARGET_FILE on the remote server
REMOTE_BACKUP_SOURCE_FILE='~/*conda-ks.cfg'
REMOTE_BACKUP_TARGET_FILE='~/tmp/ccconda-ks.cfg.bak' # For recovery

# Overwrite the REMOTE_OVERWRITE_TARGET_FILE with REMOTE_OVERWRITE_SOURCE_FILE
REMOTE_OVERWRITE_SOURCE_FILE=$REMOTE_UPLOAD_FILE
REMOTE_OVERWRITE_TARGET_FILE=$REMOTE_BACKUP_SOURCE_FILE

# ************************************************************ Bash logic
REMOTE_BACKUP_COMMAND="cp $REMOTE_BACKUP_SOURCE_FILE $REMOTE_BACKUP_TARGET_FILE -f"
REMOTE_OVERWRITE_COMMAND="cp $REMOTE_OVERWRITE_SOURCE_FILE $REMOTE_OVERWRITE_TARGET_FILE -f"

set -e

# Choice function to interact with the user
ask() {
  local prompt="${1:-Are you sure? (y/n): }"
  while true; do
    read -p "$prompt" user_choice
    case "$user_choice" in
      [Yy]) return 0 ;;
      [Nn]) echo "Aborted by user."; exit 1 ;;
      *) echo "Please enter y or n." ;;
    esac
  done
}

remote_execute() {
  sshpass -p "$LOGIN_PWD" ssh "$LOGIN_USER@$SERVER_IP" "$1"
}

if [ "$1" == "recover" ]; then
  # Define the recovery command
  RECOVER_COMMAND="cp $REMOTE_BACKUP_TARGET_FILE $REMOTE_OVERWRITE_TARGET_FILE -f"
  ask "Does the command '$RECOVER_COMMAND' look correct to you? (y/n): "
  remote_execute "$RECOVER_COMMAND"
  echo "Recovery command executed."
  exit 0  # Exit after recovery is done
fi



# 1. Upload a local file to the server
echo "[START]==================================== Upload the local file"
echo "Local file info:"
ls -al "$LOCAL_FILE"
ask "Does the command 'sshpass -p \"$LOGIN_PWD\" rsync -avz \"$LOCAL_FILE\" \"$LOGIN_USER@$SERVER_IP:$REMOTE_UPLOAD_FILE\"' look correct to you? (y/n): "
sshpass -p "$LOGIN_PWD" rsync -avz "$LOCAL_FILE" "$LOGIN_USER@$SERVER_IP:$REMOTE_UPLOAD_FILE"
echo
echo "Upload completed, Print the remote uploaded file: $REMOTE_UPLOAD_FILE"
remote_execute "ls -al $REMOTE_UPLOAD_FILE"
ask "Is the local file successfully uploaded? (y/n): "
echo "[END  ]==================================== Upload the local file"
echo

# 2. Back up the server file
echo "[START]==================================== Backup the server file"
echo "Remote back up file info:"
remote_execute "ls -al $REMOTE_BACKUP_SOURCE_FILE"
ask "Does the command '$REMOTE_BACKUP_COMMAND' look correct to you? (y/n): "
remote_execute "$REMOTE_BACKUP_COMMAND"
echo
echo "Backup completed, Print the remote backup file: $REMOTE_BACKUP_TARGET_FILE"
remote_execute "ls -al $REMOTE_BACKUP_TARGET_FILE"
ask "Is the server file successfully backed up? (y/n): "
echo "[END  ]==================================== Backup the server file"
echo

# 3. Overwrite the server files with the local file
echo "[START]==================================== Overwrite the server file"
echo "Remote source file info:"
remote_execute "ls -al $REMOTE_OVERWRITE_SOURCE_FILE"
ask "Does the command '$REMOTE_OVERWRITE_COMMAND' look correct to you? (y/n): "
remote_execute "$REMOTE_OVERWRITE_COMMAND"
echo
echo "Overwrite completed, Print the remote overwritten file:"
remote_execute "ls -al $REMOTE_OVERWRITE_TARGET_FILE"
#ask "Is the server file successfully overwritten? (y/n): "
echo "[END  ]==================================== Overwrite the server file"

# You can add post-execution commands here once all steps are complete.

# Do something