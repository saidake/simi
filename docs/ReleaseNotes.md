# Table of Contents
[Back to Main Project README](../README.md)
- [Simi](#simi)
  - [Simi 1.2.0 Release - OneDrive Document Migration](#simi-120-release---onedrive-document-migration)
    - [Core Updates](#core-updates)
    - [SpringBoot\_v1.1.0.pdf](#springboot_v110pdf)
    - [Java\_v1.1.0.pdf](#java_v110pdf)
    - [Node\_v1.1.0.pdf](#node_v110pdf)
    - [Config\_v1.1.0.pdf](#config_v110pdf)
  - [Simi 1.1.0 Release - Sandbox Module](#simi-110-release---sandbox-module)
# Simi
## Simi 1.3.0 Release - Simi Toolkit
* Updated Japanese_v1.1.0.pdf from version 1.0.0.
### SpringBoot_v1.2.0
* **SpringBoot / Third Party Package / Extensions / junit / \[mockito-core\] / Mockito**
  * Use `doReturn` to return different values on consecutive calls.  
### Java_v1.2.0
* **Java / java.base / java.util / concurrent / locks / AbstractQueuedSynchronizer**
  * Add a usage example for the `newCondition()` method.
### Linux_v1.1.0
* **Linux / Bash Script / Core**
  * Provide detailed explanation for commands like `make`, `make clean`, `make install` and `set`.
* **Linux / Bash Script / Statement / Function**
  * Provide usage examples for a bash function. 
* **Linux / Bash Script / Statement / Control Flow / while**
  * Provided usage examples for `IFS=`. 
* **Linux / Commands / Service / Service File**
  * Introduce options for configuring a systemctl service file.
### Japanese_v1.1.0
* **Japanese / Verb / 簡体**
  * Provide a comprehensive explanation of the plain form.
* **Japanese / Verb / 敬体**
  * Provide a comprehensive explanation of the polite form.

## Simi 1.2.0 Release - OneDrive Document Migration
### Core Updates
* Migrated documents from OneDrive to Git.
* Updated SpringBoot_v1.1.0.pdf, Java_v1.1.0.pdf, Node_v1.1.0.pdf, Config_v1.1.0.pdf from version 1.0.0.
* Renamed module `simi-common-util` to `simi-common-utils`.
* Renamed app `simi-memory-management` to `simi-review-tool`.
* Added several library tests to `simi-lib-test`.

### SpringBoot_v1.1.0.pdf
* **SpringBoot / Third Party Package / spring-boot-starter-security**
  * Configure SecurityFilterChain for the new version of spring security. 
* **SpringBoot / org.springframework (spring) / web / annotation / RequestMapping**
  * Jackson is used for request bodies (`@RequestBody`) and response serialization (`@ResponseBody`).
* **SpringBoot / org.springframework (spring) / web / annotation / InitBinder**
  * Added usage examples for InitBinder annotation. 
* **SpringBoot_v1.1.0.pdf / Third Party Package / Servers / Temporal**
  * Introduce the core classes in the `temporal-serviceclient` and `temporal-sdk` dependencies.
* **SpringBoot_v1.1.0.pdf / Third Party Package / Extentions / junit / \[junit-jupiter-api\]**
  * Using `@ExtendWith` in Junit 5 and `@Rule` in Junit 4.

### Java_v1.1.0.pdf
* **Java / Concept / Java Environment / Version Features / Java 16 (March 221)**
  * Using the `record` operator in Java 16 and later.
* **Java / java.base / java.time / LocalDate**
  * Introduce the default date string format.
* **Java / java.base / java.text / SimpleDateFormat**
  * Add examples for "MM" and "dd" time format.
  * SimpleDateFormat is not thread-safe. 
* **Java / java.base / java.util / (Data Type) / (List) / List**
  * How to convert a primitive array to a list  
* **Java / java.base / java.util / (Data Operation) / Arrays**
  * `Arrays.stream` has a time complexity of O(n). 

### Node_v1.1.0.pdf
* **Node / React / Core / Import Components**
  * How to import various components in React.
* **Node / Configuration / tsconfig.json**
  * Introduce typescript configuration file. 
* **Node / Npm Packages / Cli Utils / craco**
  * Use craco to overwrite webpack configurations in a CRA project. 
* **Node / Npm Packages / Loader / Cli Utils / create-react-app**
  * Build-in Support in `create-react-app` package. 
* **Node / Npm Packages / Loader / less-loader**
  * Use styles in a React component.
* **Node / Npm Packages / Loader / sass-loader**
  * Use styles in a React component.

### Config_v1.1.0.pdf
* **Config / Work Environment / git / Configuration / .gitignore**
  * Explain the priority and rules for ignoring files in  `.gitignore`. 
* **Config / Work Environment / Nodejs / Commands / yarn**
  * Commonly used yarn commands.

## Simi 1.1.0 Release - Sandbox Module
- Added a detailed algorithm explanation file `docs/Algorithms.md`.
- Optimized the deployment script, moved it to the `scripts` directory, and added additional helper bash scripts.
- Introduced the `simi-sandbox` module for system architecture exploration.
- Added an AWS test module, `simi-sandbox/simi-servers/simi-aws`, for testing AWS services.
- Added test modules for Temporal and CAP frameworks.
- Removed outdated modules: `simi-mysql`, `simi-nodb`, and `simi-oracle`.  