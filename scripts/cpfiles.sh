#!/bin/bash
# ************************************************************************************
# This script automates the process of uploading files from a local 'assets' directory
# to a remote server specified in a configuration file 'file-paths.properties'. It uses
# SSH and SCP for secure file transfer on the remote server after the upload is complete.
# Usage:
#   1. Define the file-paths.properties mapping file to specify the copy target directory
#        of each asset file.
#   2. Put your files in the assets folder.
#   3. Modify the common variables of this bash file to specify paths and update the
#        servers.sh path if needed.
#
# Since: Oct 4, 2024
# ************************************************************************************

# Common variables
# Load file-to-directory mappings from properties file
properties_file="./AAAConfig/file-paths.properties"
# Assets
assets_directory="./assets"

# Import global environment variables
source ./AAAConfig/servers.sh

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

            echo "Uploading $local_path to $TEST_AMAZON_USER@$TEST_AMAZON_IP:$remote_path"
            # Create the target directory on the remote server if it doesn't exist
            # shellcheck disable=SC2029
            ssh "$TEST_AMAZON_USER@$TEST_AMAZON_IP" "mkdir -p $remote_path"
            # Upload the file to the corresponding directory on the remote server
            scp "$local_path" "$TEST_AMAZON_USER@$TEST_AMAZON_IP:$remote_path"
        else
            echo "No target directory found for $file_name. Skipping file."
        fi
    done
}

# Execute the functions
load_properties
upload_files
