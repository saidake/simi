# Table of Contents
[Back to Main Project README](../README.md)
- [Database](#database)
  - [Oracle 23ai](#oracle-23ai)
- [Server](#server)
  - [Temporal](#temporal) 
# Database
## Oracle 23ai
Reference:  
  * https://www.oracle.com/cn/database/technologies/oracle-database-software-downloads.html

1. Pull the Oracle 23ai Docker Image  
     Run the below command in terminal.
     ```text
     docker pull container-registry.oracle.com/database/free:23c
     ```
    "Oracle 23ai" is essentially the same database as "Oracle 23c" but with a name change to emphasize the significant focus on Artificial Intelligence (AI) features included in this release.
2. Run the Oracle 23ai Container
    ```text
    docker run -d --name oracle23c-ai \
    -p 1521:1521 -p 5500:5500 \
    -e ORACLE_PWD=<your-password>> \
    container-registry.oracle.com/database/free:23c
    ```
    `ORACLE_PWD` is an environment variable used to set the initial password for the default administrative users (SYS, SYSTEM, and other privileged accounts)
3. Open a terminal session inside the container
    ```text
    docker exec -it <container-name-or-id>  bash
    ```
# Server
## Temporal
1. Prerequisites
    * Docker: Ensure Docker is installed and running.
    * Docker Compose: Make sure you have Docker Compose installed.
2. Download the Official Temporal Docker Compose File
    ```text
    git clone https://github.com/temporalio/docker-compose.git
    cd docker-compose
    ```
3. Start the Temporal Server  
   Run the following command to start all required services:
   ```text
    docker-compose up
    ```
4. Access the Temporal Web UI  
   Once the server is running, access the Temporal Web UI in your browser:
    ```text
    http://localhost:8080
    ```
