---
title: Bytecode Persistence Strategy
description: Comparison of storage formats for the Virtual Machine bytecode.
sidebar:
  label: Data Persistency
  order: 3
---

## Comparative Study

| Format | **Serialized Binary** | **Structured Text (JSON/YAML)** | **Custom Text Format** | **In-Memory** |
| :--- | :--- | :--- | :--- | :--- |
| **Type** | Compact Machine Code | Human-Readable Data | Manual String Representation | RAM Structure |
| **Size** | **Smallest.** Highly optimized. | Large (Syntax overhead). | Moderate. | N/A. |
| **I/O Speed** | **Fastest.** Direct memory mapping. | Slow (Parsing required). | Slow (Parsing required). | Instant (No I/O). |
| **Readability** | **None.** Requires disassembler. | High. | Moderate. | N/A. |
| **Implementation** | **Trivial** (Haskell `Data.Binary`). | Easy (`Aeson` library). | **Hard** (Requires new parser). | None. |

### 1. In-Memory

* **Pros:** Instant execution if the Compiler and VM are in the same binary.
* **Cons:** **Not Persistent.** It fails the project requirement to produce an executable artifact. We cannot distribute the compiled code or run it later without recompiling.

### 2. Custom Text Format

* **Pros:** We control the syntax completely.
* **Cons:** **Inefficient.** We would need to write *another* parser just to load the bytecode into the VM. It introduces unnecessary complexity and point of failure.

### 3. Structured Text (JSON/YAML)

* **Pros:** Easy to debug; human-readable by default.
* **Cons:** **Bloated.** A simple instruction like `PUSH 42` becomes `{"op": "PUSH", "arg": 42}`, wasting disk space and I/O bandwidth. It is too slow for a performant VM.

### 4. Serialized Binary

* **Pros:** Extremely compact and fast to load. It represents the exact state of the instructions in memory.
* **Cons:** Not human-readable (requires a separate disassembler tool).

---

## Why We Chose Binary Serialization

We selected **Serialized Binary Files** as the storage format for our bytecode to strictly adhere to the project specifications and maximize performance.

### 1. Project Compliance

The subject explicitly states:
> *"Your compiler should be able to output its result as a binary format (bytecode)."*
> *"Your VM should be able to load this binary format and run it."*

Using text or memory would technically fail this mandatory requirement.

### 2. Haskell Ecosystem (`Data.Binary`)

Since we are using Haskell, we can utilize the `Data.Binary` (or `Cereal`) libraries.

* We can derive the serialization logic automatically using `Generic`.
* This gives us robust, crash-proof serialization with **zero boilerplate code**.
* **Efficiency:** It handles endianness and bit-packing automatically, ensuring our bytecode is portable across architectures.

### 3. Performance

Binary files are significantly smaller than text files.

* **Loading:** The VM can "slurp" the binary file directly into data structures without the overhead of parsing text, checking for syntax errors, or converting strings to integers.
* **Execution:** This ensures the VM startup time is minimized.

### 4. Security & Integrity

While not encrypted, a binary format prevents accidental modification by users (e.g., deleting a parenthesis in a JSON file) which could crash the VM. To read the code, we satisfy the "Disassembly" requirement by implementing a specific `--debug` or `--disassemble` flag in our compiler.
