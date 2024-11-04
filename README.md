# Table of Contents
- [Introduction](#introduction)
- [Directory Structure](#directory-structure)
  - [simi-algorithm](#simi-algorithm)
  - [simi-test-maven](#simi-test-maven)
  - [simi-test-gradle](#simi-test-gradle)
# Introduction
[Back to Top](#table-of-contents) 

Simi is an educational project focused on practical experience in building and managing modular applications. It features various modules, each serving a unique purpose, enabling exploration of core programming concepts, application architecture, and integration techniques.
# Directory Structure
[Back to Top](#table-of-contents)
## AAA 
A resource directory containing startup configuration files needed by other modules.
## scripts
A folder containing bash and batch files required by other modules.
* cpfiles.sh
  - Copy local files to remote server.
* execr.sh
  - Execute a remote bash script with its environment variables.
* simidep.sh 
  - Deploy a jar file to remote server.
##  simi-algorithm
Simi Algorithm is a collection of solutions to various algorithm problems, each accompanied by detailed comments, explanations, and illustrative diagrams.  
For more information, check out the [Algorithms README](simi-algorithm/README.md).
## simi-test-gradle
A Gradle learning project that includes comprehensive testing for educational purposes.
## simi-test-maven
Maven learning modules that contain JPA, MyBatis, and various database test modules.
### simi-cap  
Simi Cap is a CAP demo application.  
The [SAP Cloud Application Programming Model (CAP)](https://cap.cloud.sap/docs/java/getting-started) is a framework of languages, libraries, and tools for building enterprise-grade services and applications.
## simi-app
Some simple applications that includes specific functionalities.
### simi-initializer-app  
Simi Initializer is an IntelliJ IDEA plugin designed to streamline the initial configuration of Maven projects with complex local setups.  
It automates startup configurations like adjusting certificate file locations, adding local-specific dependencies, and modifying server IP addresses.
### simi-sgz  
Simi Sgz is an automation script module designed for the game "Three Kingdoms Tactics".
## simi-common
Common modules shared by other modules.
## simi-config
Common configuration files for use by other modules.
## simi-custom
Custom UI modules.
## simi-gateway
Gateway modules.
## simi-node
A dedicated module to test Node-related dependencies and functionalities. This module provides a streamlined testing environment for Node.js projects.
## simi-parent
A parent module that manages the dependency versions of other modules.
## simi-service
A simple backend service for foreign exchange trading.