# Table of Contents
[Back to Main Project README](../README.md)
- [Database](#database)
  - [Oracle](#oracle)
# Database
## Oracle

### Run Oracle 23ai in Docker
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
3. Create a new database


