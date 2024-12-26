# Table of Contents
- [Introduction](#introduction)
- [Directory Structure](#directory-structure)
  - [docs](#docs)
    - [Algorithms.md](#algorithmsmd)
    - [Env.md](#envmd)
    - [Standards.md](#standardsmd)
  - [Simi Sandbox](#simi-sandbox)
# Introduction
[Back to Top](#table-of-contents) 

Simi is an educational project focused on practical experience in building and managing modular applications. It features various modules, each serving a unique purpose, enabling exploration of core programming concepts, application architecture, and integration techniques.
# Directory Structure
[Back to Top](#table-of-contents)
## docs
### Algorithms.md
This markdown file contains solutions to various algorithm problems, each with detailed comments, explanations, and illustrative diagrams.    
For more information, check out the [Algorithms File](docs/Algorithms.md).

### Env.md
This file provides details about the project's environment setup, 
including information about the virtual machines used and instructions for configuring the local development environment.  
For more information, check out the [Env File](docs/Env.md).

### Standards.md
This file outlines the standards and guidelines for the project, including JIRA issue type conventions and other best practices to ensure consistency and clarity across the team.  
For more information, check out the [Standards File](docs/Standards.md).

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
## simi-sandbox
The simi-sandbox is a dedicated Maven module designed as a versatile environment for testing, experimenting, and showcasing various code implementations.
### simi-cap  
Simi Cap is a CAP demo application.  
The [Cloud Application Programming Model (CAP)](https://cap.cloud.sap/docs/java/getting-started) is a framework of languages, libraries, and tools for building enterprise-grade services and applications.
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
Custom modules.
## simi-gateway
Gateway modules.
## simi-node
A dedicated module to test Node-related dependencies and functionalities. This module provides a streamlined testing environment for Node.js projects.
## simi-parent
A parent module that manages the dependency versions of other modules.
## simi-service
A simple backend service for foreign exchange trading.