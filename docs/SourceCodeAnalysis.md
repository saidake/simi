# Table of Contents
[Back to Main Project README](../README.md)
- [Table of Contents](#table-of-contents)
- [Server](#server)
  - [Temporal](#temporal)
    - [Startup Process](#startup-process)
      - [References](#references)
      - [Code Flow Diagram](#code-flow-diagram)
      - [Potential Issues](#potential-issues)
      - [Mermaid Source Code](#mermaid-source-code)
# Server
## Temporal
### Startup Process
#### References
* **SpringBoot.pdf / Third Party Package / Servers / Temporal**
#### Code Flow Diagram
[![](https://mermaid.ink/img/pako:eNqNlN1uozAQhV9l5KtWCpFC0l5wUS2FVqrUaquSVaUtuXBgmlgBzNqmEUry7mtiE8iPukskFNsn3xlmTtiQhKdIPLIQtFzCNIwL0Jes5mYjUljCyGz6o493LlafGV9HKL5YgpGq5nIGjnO3DQRShXILvnv14UMjhEetBCuFRju7NiQs0ks-rtm873yCjGGhThzuTxys6Hv22GwGho3ikSaKi3oGx-zgwEYBraZPbj7n9Imlj39sQC5piR4ofeqoAWR0jpkHMTEeQIsU5kzfKBS4hrWxUhxizaCgqFzBnworjAnsDDYcdUWdFHNeyo39jftNKcZ0KHDBpELRdvspLzPMdS-pYryY1iXKXhHj_wf6iWJfTDGUx8ge7jCmS89way0n_7S0IxpKRYW6uj7hm6_Th5fXn2_-cwdL6qxHmmJeckGzfVRRGIaNvAvOEH5J1DOrYejc6XDakJ6dBPYkcJtI6Sy0mdgvw9HRsq2pHfA-hW-2fXbUOo6he1nQNVhrxn3fbdR0QvYQEzIgOYqcslT_zzeNOCZqqacSk-bxUypWMYmLndbRSvGoLhLiKVHhgAheLZbE-6SZ1KuqTHWCQ0b1qPLDbkmL35x3a0yZHsmLea3s3y67v73zZ8A?type=png)](https://mermaid.live/edit#pako:eNqNlN1uozAQhV9l5KtWCpFC0l5wUS2FVqrUaquSVaUtuXBgmlgBzNqmEUry7mtiE8iPukskFNsn3xlmTtiQhKdIPLIQtFzCNIwL0Jes5mYjUljCyGz6o493LlafGV9HKL5YgpGq5nIGjnO3DQRShXILvnv14UMjhEetBCuFRju7NiQs0ks-rtm873yCjGGhThzuTxys6Hv22GwGho3ikSaKi3oGx-zgwEYBraZPbj7n9Imlj39sQC5piR4ofeqoAWR0jpkHMTEeQIsU5kzfKBS4hrWxUhxizaCgqFzBnworjAnsDDYcdUWdFHNeyo39jftNKcZ0KHDBpELRdvspLzPMdS-pYryY1iXKXhHj_wf6iWJfTDGUx8ge7jCmS89way0n_7S0IxpKRYW6uj7hm6_Th5fXn2_-cwdL6qxHmmJeckGzfVRRGIaNvAvOEH5J1DOrYejc6XDakJ6dBPYkcJtI6Sy0mdgvw9HRsq2pHfA-hW-2fXbUOo6he1nQNVhrxn3fbdR0QvYQEzIgOYqcslT_zzeNOCZqqacSk-bxUypWMYmLndbRSvGoLhLiKVHhgAheLZbE-6SZ1KuqTHWCQ0b1qPLDbkmL35x3a0yZHsmLea3s3y67v73zZ8A)
#### Potential Issues
* The workflow execution method is blocked.  
  
  No available worker can be found in the binding task queue of the current workflow.
  The workflow may be bound to the wrong task queue that lacks any workers, causing it to be blocked.
* The workflow has been triggered and displayed in the temporal UI, but the worker is not responding.
* The delay method doesn't work and executes immediately.
#### Mermaid Source Code
```text
graph TD
    subgraph Step 1
    A1[WorkflowServiceStubs] -->|Creates| A2([A Work Flow Service Stub])
    end
    subgraph Step 2
    B1[WorkflowClient] -->|Creates| B2([A Work Flow Client])
    end
    subgraph Step 3
    C1[WorkerFactory]  -->|Creates| C2([A Worker Factory])
    end


    subgraph Step 4
    C3@{ shape: trap-t, label: "Create and bind a new worker to \n a task queue" }
    D1([A Worker])
    end

    subgraph Step 5
    D2@{ shape: trap-t, label: "worker.registerWorkflowImplementationTypes" }
    D3@{ shape: trap-t, label: "worker.registerActivitiesImplementations" }
    end
    
    subgraph Step 6
    D4@{ shape: trap-t, label: "workerFactory.start()" }
    end

    TEMPORAL@{ shape: cyl, label: "Temporal Server" }

    A2 -. Used by .-> B1
    B2 -. Used by .-> C1
    C2 --> C3
    C3 --> D1
    C3 --> TEMPORAL
    D1 -->|Register Workers| D2
    D1 -->|Register Activities| D3
    C2 -->|Starts Workers| D4
```

