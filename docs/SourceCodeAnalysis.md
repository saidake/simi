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
[![](https://mermaid.ink/img/pako:eNqNlEFvm0AQhf_KaE-JZCwZuz1wiEqgkSo1ahVcVWrwYQ0Te2Vg6e4SC9n-713YxYBrJfHB8g6Pb4Z5zxxIwlMkHtkIWm5hGcYF6I-s1qYQKSxhZor-7Pk3F7uXjO8jFK8swUhVa7kCx7k7BgKpQnkE37159qERwoNWgpVCo13dGhIW6bU-rine932CjGGhLjrcX3SworfZc1MMDBvFA00UF_UKxuzgzEYBnWZItpj5lwPILS3RA6WbOGoCGV1j5kFMDAxokcKa6S8KBe5hb5iKQ6wZFBSVO_hbYYUxgVMHHg-9MMVw1g91ZZjxPZ_sPe4bE5pZpgI3TCoU3ba_5WWGud4lVYwXy7pEaWZrgfOPA_1EsVemGMoxcoA723TtGT7blot3W1qLplJRoW5uL_jm5_Lr488fT_73HpbU2YC0xLzkgmZtVFEM7fBdcKbwS6K2soapc6fDaUP635XAXgncJlI6Il1U2mM4Gx27mTqD2xQ-2fVZq3UcQ_e6oF-w1syHfY9Rswk5QCzIhOQocspS_T8_NOKYqK12JSbN46dU7GISFyeto5XiUV0kxFOiwgkRvNpsifdCM6lPVZnqYIeMaqvyc7WkxR_O-zOmTFvyaF4r7dvl9A_Bq2fA?type=png)](https://mermaid.live/edit#pako:eNqNlEFvm0AQhf_KaE-JZCwZuz1wiEqgkSo1ahVcVWrwYQ0Te2Vg6e4SC9n-713YxYBrJfHB8g6Pb4Z5zxxIwlMkHtkIWm5hGcYF6I-s1qYQKSxhZor-7Pk3F7uXjO8jFK8swUhVa7kCx7k7BgKpQnkE37159qERwoNWgpVCo13dGhIW6bU-rine932CjGGhLjrcX3SworfZc1MMDBvFA00UF_UKxuzgzEYBnWZItpj5lwPILS3RA6WbOGoCGV1j5kFMDAxokcKa6S8KBe5hb5iKQ6wZFBSVO_hbYYUxgVMHHg-9MMVw1g91ZZjxPZ_sPe4bE5pZpgI3TCoU3ba_5WWGud4lVYwXy7pEaWZrgfOPA_1EsVemGMoxcoA723TtGT7blot3W1qLplJRoW5uL_jm5_Lr488fT_73HpbU2YC0xLzkgmZtVFEM7fBdcKbwS6K2soapc6fDaUP635XAXgncJlI6Il1U2mM4Gx27mTqD2xQ-2fVZq3UcQ_e6oF-w1syHfY9Rswk5QCzIhOQocspS_T8_NOKYqK12JSbN46dU7GISFyeto5XiUV0kxFOiwgkRvNpsifdCM6lPVZnqYIeMaqvyc7WkxR_O-zOmTFvyaF4r7dvl9A_Bq2fA)

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

