# Table of Contents
- [Introduction](#introduction)
- [Directory Structure](#directory-structure)
  - [docs](#docs)
    - [Env.md](#envmd)
    - [ReleaseNotes.md](#releasenotesmd)
  - [AAA](#aaa)
  - [simi-common](#simi-common)
    - [simi-common-utils](#simi-common-utils)
  - [sandbox](#sandbox)
    - [extension-test](#extension-test)
      - [simi-cap](#simi-cap)
  - [simi-app](#simi-app)
    - [simi-initializer-app](#simi-initializer-app)
    - [simi-sgz](#simi-sgz)
  - [simi-config](#simi-config)
  - [simi-custom](#simi-custom)
  - [simi-gateway](#simi-gateway)

# Introduction
[Back to Top](#table-of-contents)

**Simi Sandbox** is a modular Gradle project used to explore architecture and integration techniques.  
It includes documentation and tools to help with configuration and usage.

# Directory Structure
[Back to Top](#table-of-contents)

## docs

### Env.md
Describes the local runtime environment.  
See [Env.md](docs/Env.md) for details.

### ReleaseNotes.md
Contains release notes.  
See [ReleaseNotes.md](docs/ReleaseNotes.md) for details.

## AAA
Startup configuration files used by other modules.

## simi-common

### simi-common-utils

- `StopWatchDebugger`  
  Utility for measuring sync and async execution times without modifying original code.  
  Run its `main` method to see output.  
  Works in multi-threaded environments using a singleton `stopwatch`.

  ![](docs/assets/main/swt1.png)

  Usage: Copy the class into your project and debug using breakpoints in **IDEA**.  
  ![](docs/assets/main/swt2.png)

- `IpUtilsServlet`  
  Gets client IPs in servlet-based web apps.

- `IpUtilsWebFlux`  
  Gets client IPs in WebFlux-based apps.

## sandbox
A Maven module for testing, experimenting, and showcasing code.

### extension-test

#### simi-cap
CAP demo app.  
Based on the [SAP Cloud Application Programming Model (CAP)](https://cap.cloud.sap/docs/java/getting-started).

## simi-app

### simi-initializer-app
An IntelliJ IDEA plugin that simplifies Maven project setup.  
It handles certificate paths, local dependencies, and server IPs.

### simi-sgz
Automation scripts for the game [Three Kingdoms Tactics](https://sangokushi.qookkagames.jp).

## simi-config
Shared configuration files.

## simi-custom
Custom modules.

## simi-gateway
Gateway-related modules.
