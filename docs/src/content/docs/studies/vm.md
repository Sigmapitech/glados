---
title: Virtual Machine Architecture Strategy
description: A comparison between Stack-Based and Register-Based Virtual Machines, explaining our architectural choice.
sidebar:
  label: VM Strategy
  order: 4
---

## Comparative Study

| Feature | **Stack VM** | **Register VM** |
| :--- | :--- | :--- |
| **Examples** | Python VM, JVM | Lua VM, Erlang BEAM |
| **Execution Model** | Operates on an implicit stack | Operates on explicit registers |
| **Instruction Simplicity** | **High.** Very small, simple instructions | **Lower.** Instructions encode more information |
| **Instruction Count** | **High.** More instructions for simple tasks | **Low.** Fewer instructions needed |
| **Performance** | Moderate. Extra stack manipulation overhead | **High.** Fewer memory accesses |
| **Compiler Complexity** | **Low.** Easy to generate code | Higher. Requires register allocation |
| **VM Complexity** | **Low.** Simple interpreter loop | Moderate. Instructions are richer |
| **Maintainability** | High. Easy to reason about | Moderate. More moving parts |

---

### 1. Stack-Based Virtual Machine

A stack VM uses an implicit operand stack. Instructions do not name operands
explicitly; instead, they consume values from the top of the stack and push
results back.

**Typical code:**

```
PUSH a
PUSH b
ADD
POPRET
```

* **Pros:**
  * **Simplicity.** Instructions are small and uniform.
  * Easy to implement and debug.
  * Well-suited for simple compilers and educational languages.

* **Cons:**
  * **Instruction-heavy.** Even simple expressions require multiple instructions.
  * Frequent stack pushes and pops introduce runtime overhead.
  * Harder to optimize aggressively due to implicit data flow.

Stack VMs are ideal when implementation speed and conceptual clarity are more
important than raw performance.

---

### 2. Register-Based Virtual Machine

A register VM operates on a fixed set of registers. Instructions explicitly
specify their input and output operands.

**Typical code:**

```
ADD out, a, b
```

* **Pros:**
  * **Performance.** Fewer instructions and reduced memory traffic.
  * More explicit data flow enables better optimizations.
  * Instructions are more expressive and versatile.

* **Cons:**
  * Instructions are more complex.
  * The compiler must manage register allocation.
  * Slightly more complex VM implementation.

Register VMs trade simplicity for efficiency, making them well-suited for
performance-critical systems.
