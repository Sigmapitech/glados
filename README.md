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

#### Option 1: Using Nix (Recommended)

- [Install Nix](https://nixos.org/download/) (if you haven't already)
- Enter the development environment:
  ```shell
  nix develop
  ```

#### Option 2: Using Docker

Build and run the project in a Docker container without installing dependencies:

```shell
# Build the Docker image
docker build -t glados .

# Run the container
docker run -it --rm glados

# Or run with volume mount for development
docker run -it --rm -v $(pwd):/app glados
```

#### Option 3: Manual Installation

Install the following system dependencies:

- `GHC` (version `9.8.4`)
- `cabal` (version `3.14.2.0`)
- `make` (version `4.0+`)

**On Ubuntu/Debian:**
```shell
sudo apt update
sudo apt install -y ghc cabal-install make
cabal update
```

**On Fedora**
```shell
sudo dnf install -y ghc cabal-install make
cabal update
```

**On macOS:**
```shell
brew install ghc cabal-install
cabal update
```

**On Arch Linux:**
```shell
sudo pacman -S ghc cabal-install make
cabal update
```

> [!TIP]
> After installing GHC and Cabal, all Haskell package dependencies (like `hspec`, `megaparsec`, `directory`, etc.) will be automatically downloaded and installed by Cabal when you run `make` or `cabal build all`.

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
