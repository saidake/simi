# JIRA TYPE
<table>
<thead>
<tr>
    <th>JIRA TYPE</th>
    <th>Description</th>
    <th>Example</th>
</tr>
</thead>
<tbody>
<tr>
    <td>feat (Feature)</td>
    <td>Represents a new feature or functionality being added to the product. These are typically significant user-facing changes.</td>
    <td>Implement user login functionality.</td>
</tr>
<tr>
    <td>fix</td>
    <td>Refers to a bug fix. These changes resolve issues or defects found in the code or system.</td>
    <td>Fix broken navigation links on the homepage.</td>
</tr>
<tr>
    <td>docs (Documentation)</td>
    <td>Involves adding or updating documentation, whether it’s for the codebase, APIs, or user guides.</td>
    <td>Update API documentation for new endpoints.</td>
</tr>
<tr>
    <td>style</td>
    <td>Refers to changes that do not affect the logic or functionality of the code but improve its formatting, structure, or readability (e.g., code indentation, formatting, linting).</td>
    <td>Fix code indentation in Main.java.</td>
</tr>
<tr>
    <td>refactor</td>
    <td>Changes in the code to improve its internal structure without altering external behavior. These tasks aim to make the code more maintainable and cleaner.</td>
    <td>Refactor login service for better modularity.</td>
</tr>
<tr>
    <td>perf (Performance)</td>
    <td>Focuses on improving performance, such as optimizing algorithms or reducing resource usage.</td>
    <td>Optimize database query for faster response times.</td>
</tr>
<tr>
    <td>test</td>
    <td>Adding or updating tests to ensure the code works as expected. This category covers unit tests, integration tests, and other testing frameworks.</td>
    <td>Add unit tests for user authentication.</td>
</tr>
<tr>
    <td>build</td>
    <td>Changes related to the build system, dependencies, or build scripts (e.g., Maven, Gradle).</td>
    <td>Update Gradle build configuration.</td>
</tr>
<tr>
    <td>ci (Continuous Integration)</td>
    <td>Changes to CI/CD (Continuous Integration/Continuous Deployment) pipelines, such as updating build workflows or modifying test automation.</td>
    <td>Update CI pipeline to run additional test cases.</td>
</tr>
<tr>
    <td>chore</td>
    <td>Routine tasks that do not modify the production code directly but are necessary for project maintenance, such as updating dependencies or cleaning up the project.</td>
    <td>Upgrade project dependencies.</td>
</tr>
<tr>
    <td>revert</td>
    <td>Reverts a previous commit or change, effectively undoing it.</td>
    <td>Revert commit 123abc due to regression issues.</td>
</tr>
</tbody>
</table>

# Ongoing JIRA
* [simi-webflux] feat: create dynamodb tables
* [simi-webflux] build: execute commands via ssh and scp
* [simi-webflux] feat: invoke lambda functions from LocalStack
* [simi-webflux] build: upload lambda jar to LocalStack and invoke it
* [simi-webflux] feat: build LocalStack virtual machine server
* [simi-webflux] feat: configure Amazon Lambda
* [simi-webflux] test: Mono usage practice
* [simi] test: add test gradle module
* [simi-sgz] refactor: extract waiting time
* [simi-sgz] fix: change troop waiting time
* [simi-sgz] feat: add mutex lock to allow multiple accounts to level up at the same time
* [simi-sgz] feat: add a new member and reset coordinates
* [simi-sgz] sgz game script
* [simi-sgz] feat:Increase march time
* [oracle-test] test jpa ddl function
* [memory-enhancement] simi-memory-management
* [simi-trading] rename trading project
* [simi-trading] make FXCM api work again
* [simi-algorithm] feat: add a test data generation tool
# Next Release
### Simi Initializer 2.1.2
* Compatible with IDEA 2024
### Simi Trading 1.0.0
* Display a simple page