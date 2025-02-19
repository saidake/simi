# Table of Contents
[Back to Main Project README](../README.md)
- [Simi](#simi)
  - [Simi 1.2.0 Release - OneDrive Document Migration](#simi-120-release---onedrive-document-migration)
  - [Simi 1.1.0 Release - Sandbox Module](#simi-110-release---sandbox-module)
# Simi
## Simi 1.2.0 Release - OneDrive Document Migration
### SpringBoot_v1.1.0.pdf
* **SpringBoot / Third Party Package / spring-boot-starter-security**
  * Configure SecurityFilterChain for the new version of spring security. 
* **SpringBoot / org.springframework (spring) / web / annotation / RequestMapping**
  * Jackson is used for request bodies (`@RequestBody`) and response serialization (`@ResponseBody`).
* **SpringBoot / org.springframework (spring) / web / annotation / InitBinder**
  * Added usage examples for InitBinder annotation. 
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