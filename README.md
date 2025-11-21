# GLaDOS

## Generic Language and Data Operand Syntax

### 🔗 Useful Links

- 🎨 Design & Architecture: [Figma Board](https://www.figma.com/design/nEfFaG3XVqWuiBLY3HY8iH/)
- 📋 Project Management: [GitHub Project](https://github.com/orgs/Sigmapitech/projects/15)
- 📚 BNF Grammar: [Railroad Diagram Generator](https://www.bottlecaps.de/rr/ui)
- 🧪 BNF Playground: [Online Tester](https://bnfplayground.pauliankline.com/)

### 📋 Overview

GLaDOS is a functional programming language designed and implemented in Haskell. It features a custom LISP-like syntax (bootstrap), a stack-based virtual machine, and a compiler.

**Core Components:**

- **AST**: Abstract Syntax Tree definitions.

- **Compiler**: Transforms source code into bytecode.

- **VM**: A Stack Machine to execute the compiled bytecode.

### 🚀 Getting Started

We use **Nix Flakes** to ensure a reproducible development environment across all Linux distributions.

**1. Prerequisites**
[Install Nix](https://nixos.org/download/) (if you haven't already):

```Shell
# Multi-user installation
sh <(curl --proto '=https' --tlsv1.2 -L https://nixos.org/nix/install) --daemon
# Single-user installation
sh <(curl --proto '=https' --tlsv1.2 -L https://nixos.org/nix/install) --no-daemon
```

**2. Development Environment (`ufda`)**
To automate your environment loading, we use a helper function called `ufda` (Use Flake / Direnv Allow). Add this to your `.bashrc` or `.zshrc`:

```Shell
# ufda: Use Flake / Direnv Allow
ufda() {
    local flake_arg="$1"

    # Require flake.nix
    if [[ ! -f "flake.nix" ]]; then
        echo "❌ No flake.nix found. Aborting." >&2
        return 1
    fi

    local new_line current_line

    if [[ -z "$flake_arg" ]]; then
        # No argument → preserve what's already in .envrc
        if [[ -f ".envrc" ]]; then
            current_line="$(grep -E 'use flake' .envrc | head -n 1)"
            if [[ -n "$current_line" ]]; then
                new_line="$current_line"
            else
                new_line="use flake"
            fi
        else
            new_line="use flake"
        fi
    else
        # Explicit argument
        new_line="use flake $flake_arg"
    fi

    # Always overwrite .envrc
    printf "%s\n" "$new_line" > .envrc
    echo "✓ .envrc set to: $new_line"

    # Force environment reload
    direnv allow
}
```

**3. Usage**
Once setup, simply run this in the project root:

```Shell
ufda
```

This will automatically load `GHC`, `Cabal`, `HLS` (Haskell Language Server), `HLint`, and `Ormolu`.

### 🛠️ Build & Run

Since we are using Nix, the build process is standardized.

**Build the project:**

```Shell
cabal build all
```

**Run the LISP interpreter (Bootstrap):**

```Shell
cabal run lisp-interpreter
```

**Run the Compiler/VM:**

```Shell
cabal run glados
```

### 🧪 Testing & Quality

We use `HUnit` for testing and `hlint` for linting.

**Run Unit Tests:**

```Shell
cabal test all
```

**Check Coverage:**

```Shell
# Coverage is generated via hpc-codecov
cabal test --enable-coverage
```

**Lint Code:**

```Shell
hlint .
```

### 📂 Repository Structure

The project is organized as a multi-package Cabal project:

```Shell
.
├── flake.nix       # Dependency definitions
├── Makefile        # Epitech standard compilation rules
├── ast/            # Abstract Syntax Tree definitions
├── compiler/       # Source to Bytecode compiler
├── vm/             # Virtual Machine implementation
└── lisp/           # Bootstrap LISP interpreter
```

### 📜 License

This project is licensed under the [BSD-2-Clause License](LICENSE).
