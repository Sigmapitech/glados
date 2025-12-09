---
title: Parsing Library Research & PoCs
description: Comparison of Haskell parsing libraries (Parsec, Megaparsec, Attoparsec) and proof of concepts
---

# Parsing Lib Research & PoCs

## 1. Options Comparison

here are a comparative of all three options

### A. Parsec

- Pros: Standard, tons of tutorials online.
- Cons: * Error messages are generic strings.
  - Hard to pass custom error data (like specific error codes) up the stack.
- Verdict: Usable, but we'll struggle with the `Exit 84` requirement when we need specific error contexts.

### B. Megaparsec

- Pros: * Fast.
  - Custom Error Components: We can define our own `GladosError` (or named however you want) type and throw it inside the parser.
  - Pretty Printing: It generates nice looking error messages.
- Cons: slightly more verbose setup than Parsec (types signatures are longer).

### C. From Scratch (The "Hard Way")

- Pros: Zero dependencies.
- Cons: * Easy for Part 1 (S-Expr), but extremely hard for Part 2.
  - For Part 2, we need operator precedence, associativity, and custom syntax.
- Verdict: Not necessarily worth the time sink.

## 2. Deep Dive: Mechanics & Error Handling

Here is how they differ under the hood.

### How they work

Both libraries are Monadic Parser Combinators.

### The big Difference: Type Signature

This is where Megaparsec wins.

#### Parsec's Type: `type Parser a = Parsec String () a`

- It takes a Stream (`String`), a User State (`()`), and returns a result `a`.
- The Error is hardcoded: You can't change the error type. It's always a list of strings (`"expecting X"`, `"unexpected Y"`). You can't embed data like `InvalidType Integer` or `VariableNotFound String` easily.

#### Megaparsec's Type: `type Parser a = Parsec CustomError String a`

- The Error is polymorphic: We can define a Haskell data type:

```hs
data GladosError
    = IllegalNumber Integer
    | UndefinedSymbol String
    | SyntaxError
```

- When the parsing fails, we don't just get a string. This allows us to pattern match on the error type in `main` and what to do with it

## 3. Implementation (Megaparsec)

1. Define a custom `GladosError` data type (as shown in the PoC below).

2. Run the parser using `runParser`.

3. Match the result:
  - `Right ast` -> Continue to evaluation/compilation.
  - `Left errorBundle` ->
    - Use `errorBundlePretty` to print the nice human-readable error to `stderr`.
    - Call `exitWith (ExitFailure 84)`.

This keeps the parser pure (no IO inside the logic).

## 4. Proof of Concept

For this part please refer to files in the `poc/` directory:

- `MegaparsecPoC.hs`: A minimal Megaparsec parser that demonstrates custom error handling.
- `ParsecPoC.hs`: A minimal Parsec parser showing the limitations in error handling
