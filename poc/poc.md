# Parsing Lib Research & PoCs

Quick summary of the research on which parsing strategy we should use for the GLaDOS project.

**TL;DR**: We should use Megaparsec. It handles custom errors (required for the semantic checks) much better than Parsec, and writing it from scratch will be a nightmare for Part 2 (grammar/operator precedence).

## 1. Options Comparison

I compared the three main options available to us:

### A. Parsec

- Pros: Standard, tons of tutorials online.
- Cons: * Error messages are generic strings  (e.g., `"unexpected '('"`).
  - Hard to pass custom error data (like specific error codes) up the stack.
- Verdict: Usable, but we'll struggle with the `Exit 84` requirement when we need specific error contexts.

### B. Megaparsec

- Pros: * Fast.
  - Custom Error Components: This is the killer feature. We can define our own `GladosError` type and throw it inside the parser.
  - Pretty Printing: It generates those nice Rust/Clang style error messages with the pointer `^` to the error location automatically.
- Cons: slightly more verbose setup than Parsec (types signatures are longer).

### C. From Scratch (The "Hard Way")

- Pros: Zero dependencies.
- Cons: * Easy for Part 1 (S-Expr), but extremely hard for Part 2.
  - For Part 2, we need operator precedence (`e.g PEMDAS`), associativity, and custom syntax.
  - Libraries handle this with `makeExprParser`. If we do it from scratch, we have to manually implement Shunting-yard algorithms or Pratt parsers.
- Verdict: Not necessarily worth the time sink.

## 2. Deep Dive: Mechanics & Error Handling

To explain my choice, here is how they differ under the hood.

### How they work

Both libraries are Monadic Parser Combinators.

- Combinators: You build complex parsers by combining small ones (e.g., `parseList` is composed of `char '('`, `many parseExpr`, and `char ')'`).
- Monadic: They implement the `Monad` typeclass. This allows us to sequence operations: "Parse A, then pass the result to B, then return C".
- Backtracking: Both use `try`. If a parser consumes input but fails halfway, it doesn't automatically reset the cursor. `try` ensures that if a branch fails, we rewind the input stream to try the next branch.

### The Critical Difference: The Type Signature

This is where Megaparsec wins for our specific "Exit 84" needs.

#### Parsec's Type: `type Parser a = Parsec String () a`

- It takes a Stream (`String`), a User State (`()`), and returns a result `a`.
- The Error is hardcoded: You cannot change the error type. It is always a list of strings (`"expecting X"`, `"unexpected Y"`). You cannot embed data like `InvalidType Integer` or `VariableNotFound String` easily.

#### Megaparsec's Type: `type Parser a = Parsec CustomError String a`

- Notice the extra type parameter: `CustomError`.
- The Error is polymorphic: We can define a Haskell data type:

```hs
data GladosError
    = IllegalNumber Integer
    | UndefinedSymbol String
    | SyntaxError
```

- When parsing fails, we don't just get a string; we get this exact data structure. This allows us to pattern match on the error type in `main` and decide whether to print a warning, a fatal error, or exit with 84.

## 3. Implementation Strategy (Megaparsec)

To handle the Epitech `Exit 84` requirement cleanly without spaghetti code:

1. Define a custom `GladosError` data type (as shown in the PoC below).

2. Run the parser using `runParser`.

3. Match the result:
  - `Right ast` -> Continue to evaluation/compilation.
  - `Left errorBundle` ->
    - Use `errorBundlePretty` to print the nice human-readable error to `stderr`.
    - Call `exitWith (ExitFailure 84)`.

This keeps the parser pure (no IO inside the logic) while strictly adhering to the project guidelines.

## 4. Proof of Concept

For this part please refer to files in the `poc/` directory:

- `MegaparsecPoC.hs`: A minimal Megaparsec parser that demonstrates custom error handling.
- `ParsecPoC.hs`: A minimal Parsec parser showing the limitations in error handling
