#!/bin/bash

# Import global environment variables
source AAAconfig.sh

ssh "$TEST_AMAZON_USER"@"$TEST_AMAZON_IP" "docker-compose restart"