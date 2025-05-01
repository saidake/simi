# Table of Contents
[Back to Main Project README](../README.md)
- [Table of Contents](#table-of-contents)
- [Simi Sandbox 1.3.0 Release - Gradle Build Management](#simi-sandbox-130-release---gradle-build-management)
- [Simi Sandbox 1.2.0 Release - Module Name Updates](#simi-sandbox-120-release---module-name-updates)
- [Simi Sandbox 1.1.0 Release - Sandbox Module](#simi-sandbox-110-release---sandbox-module)
# Simi Sandbox 1.3.0 Release - Gradle Build Management
* Refactor the entire project to use Gradle for building.
* Move all technical documents to `simi-docs` project.
# Simi Sandbox 1.2.0 Release - Module Name Updates
* Migrate documents from OneDrive to Git (deprecated, all technical documents have been moved to `simi-docs` project).
* Rename module `simi-common-util` to `simi-common-utils`.
* Rename app `simi-memory-management` to `simi-review-tool`.
* Add several library tests to `simi-lib-test`.

# Simi Sandbox 1.1.0 Release - Sandbox Module
- Add a detailed algorithm explanation file `docs/Algorithms.md`.
- Optimize the deployment script, moved it to the `scripts` directory, and added additional helper bash scripts.
- Introduce the `simi-sandbox` module for system architecture exploration.
- Add an AWS test module, `simi-sandbox/simi-servers/simi-aws`, for testing AWS services.
- Add test modules for Temporal and CAP frameworks.
- Remove outdated modules: `simi-mysql`, `simi-nodb`, and `simi-oracle`.  