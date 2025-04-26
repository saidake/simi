#!/bin/bash
# ************************************************************************************
# This script automates the process of uploading all files from a local 'assets' directory
# to a remote server specified in a configuration file 'file-paths.properties'.
# It uses SSH and SCP for secure file transfer on the remote server after the upload is complete.
#
# Prerequisites:
#   1. Define the 'file-paths.properties' mapping file to specify the copy target directory
#        of each asset file.
#   2. Put your files in the 'assets' folder.
#   3. Modify the required configurations in the bash file with your own values.
#
# Usage:
#   ./cpfiles.sh
#
# Author: Craig Brown
# Date: Oct 4, 2024
# ************************************************************************************

# ================================================================== Required Configurations
# Import global environment variables
source ./config/servers.sh
REMOTE_USER=$TEST_AMAZON_USER
REMOTE_HOST=$TEST_AMAZON_IP

# Load file-to-directory mappings from properties file
properties_file="./config/file-paths.properties"
# Assets
assets_directory="./assets"


# ================================================================== Functions
# Function to read properties file and store mappings in an associative array
declare -A file_mappings
load_properties() {
    while IFS='=' read -r filename directory; do
        # Trim spaces and store in array
        filename=$(echo "$filename" | xargs)
        directory=$(echo "$directory" | xargs)
        file_mappings["$filename"]="$directory"
    done < "$properties_file"
}

# Function to upload files from assets directory
upload_files() {
    # Iterate over all files in the ./assets directory
    for local_path in "$assets_directory"/*; do
        file_name=$(basename "$local_path")

        # Check if the file exists in the properties file
        if [[ -n "${file_mappings[$file_name]}" ]]; then
            remote_path="${file_mappings[$file_name]}"

            echo "Uploading $local_path to $REMOTE_USER@$REMOTE_HOST:$remote_path"
            # Create the target directory on the remote server if it doesn't exist
            # shellcheck disable=SC2029
            ssh "$REMOTE_USER@$REMOTE_HOST" "mkdir -p $remote_path"
            # Upload the file to the corresponding directory on the remote server
            scp "$local_path" "$REMOTE_USER@$REMOTE_HOST:$remote_path"
        else
            echo "No target directory found for $file_name. Skipping file."
        fi
    done
}

# Execute the functions
load_properties
upload_files
