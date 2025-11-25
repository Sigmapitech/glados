# GLaDOS

> *Generic Language and Data Operand Syntax*

GLaDOS is a interpreted programming language implemented in Haskell that follows a C-like syntax. As part of the assignment, it includes a small lisp interpreter.

**Core Components**

- **AST**: Abstract Syntax Tree definitions.

- **Compiler**: Transforms source code into bytecode.

- **VM**: A Stack Machine to execute the compiled bytecode.


### 🔗 Useful Links

- 🎨 Design & Architecture: [Figma Board](https://www.figma.com/design/nEfFaG3XVqWuiBLY3HY8iH)
- 📋 Project Management: [GitHub Project](https://github.com/orgs/Sigmapitech/projects/15)


### Pre-requisites

> [!NOTE]
> We use **Nix Flakes** to ensure a reproducible development environment

- [Install Nix](https://nixos.org/download/) (if you haven't already)

**OR manually install:**

- `GHC` (version `9.8.4`)
- `cabal` (version `3.14.2.0`)
- `make` (version `4.0+`)

### 🛠️ Build & Run

**Build the entire project:**

```Shell
make
```

### 🧪 Testing & Quality

We use `Hspec` for testing and `hlint` for linting.

**Run Unit Tests:**

```Shell
cabal test all
```

**Check Coverage:**

```Shell
cabal test all --enable-coverage
```

**Lint Code:**

```Shell
hlint .
```
