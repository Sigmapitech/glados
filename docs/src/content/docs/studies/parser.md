---
title: Parser Implementation Strategy
description: A comparison between Parsec, Megaparsec, and a Custom Parser, explaining our decision to use Megaparsec.
sidebar:
  label: Parsing Strategy
  order: 2
---

## Comparative Study

| Feature | **Megaparsec** | **Parsec** | **Custom Parser** |
| :--- | :--- | :--- | :--- |
| **Type** | Modern Monadic Parser Combinator | Legacy Parser Combinator | Manual String Processing |
| **Error Reporting** | **Excellent.** Detailed, annotated error bundles. | Basic. Often vague source positions. | **Poor.** Requires manual implementation of tracking. |
| **Performance** | **High.** Optimized for speed and memory. | Moderate. Older optimization techniques. | Variable. Depends entirely on implementation quality. |
| **Infix Operators** | **Native.** `makeExprParser` handles precedence automatically. | Native (`buildExpressionParser`), but older API. | **None.** Requires manual Shunting-yard algorithm. |
| **Boilerplate** | **Low.** Rich standard library of lexers. | Moderate. Requires more setup for tokens. | **High.** Must reinvent basic tokenizing logic. |
| **Maintainability** | **High.** Actively maintained standard. | Low. Mostly in maintenance mode. | **Low.** High technical debt for future teams. |

### 1. Custom Parser

* **Pros:** Complete understanding of every line of code; zero dependencies.
* **Cons:** **Reinventing the wheel.** We would have to manually implement backtracking, error position tracking (line/column), and operator precedence logic. This is time-consuming and prone to bugs, risking the tight deadlines of the Glados project.

### 2. Parsec

* **Pros:** The historical standard for Haskell parsing. Stable and widely documented.
* **Cons:** **Dated.** It lacks the modern error reporting capabilities of Megaparsec. The API is less ergonomic for complex lexing tasks compared to its successor.

### 3. Megaparsec

* **Pros:** The modern industry standard. It combines the flexibility of Parsec with significantly better error messages and performance. It includes robust helper modules (`Lexer`) for handling whitespace, comments, and indentation automatically.

---

## Why We Chose Megaparsec

We selected **Megaparsec** because the Glados project requirements (Part 2) demand features that are difficult to implement manually.

### 1. Handling Operator Precedence (Part 2)

The project specification explicitly requires:
> *"Infix arithmetic operators with the expected priorities"* and *"Infix user defined operators with user defined priority"*

Implementing correct operator precedence (e.g., `*` binds tighter than `+`) manually requires implementing algorithms like Shunting-yard.

* **With Megaparsec:** We utilize `Control.Monad.Combinators.Expr`. We simply define a table of operators and their precedence, and the library handles the parsing logic automatically.

### 2. Accessibility & Error Messages

The project emphasizes accessibility:
> *"Your language must be easy to read and understand."*

A key part of a language's usability is helpful error messages when the user makes a typo.

* **Megaparsec** provides "Error Bundles" out of the box. Instead of crashing or printing `Parse Error`, it generates underlined, contextual error messages pointing to the exact line and column of the syntax error.

### 3. Separation of Concerns

Megaparsec encourages a clean separation between the **Lexer** (tokens, whitespace) and the **Parser** (grammar). This aligns with the architectural requirement to separate the frontend from the AST evaluation, ensuring code modularity.

> **Conclusion:** Megaparsec allows us to focus on the *semantics* of our language (the VM and Compiler) rather than spending weeks debugging the *syntax* parsing mechanics.
