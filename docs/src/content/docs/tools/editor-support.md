---
title: Editor Support
description: Set up syntax highlighting and code editing features for Quant in your favorite editor
---

Quant provides first-class editor support for VSCode, Vim, and Emacs. Choose your editor below to get started.

## VSCode

The official VSCode extension provides syntax highlighting, code snippets, and file icons for Quant files.

### Installation

1. Download the latest `.vsix` file from the [releases page](https://github.com/Sigmapitech/glados/releases)
2. Install the extension:

```bash
code --install-extension quant-1.1.0.vsix
```

Or install via VSCode UI:
- Press `Ctrl+Shift+P` (or `Cmd+Shift+P` on macOS)
- Type "Extensions: Install from VSIX..."
- Select the downloaded `.vsix` file

### Features

- **Syntax highlighting** for all Quant keywords, types, and operators
- **Code snippets** for common patterns (type prefix + Tab):
  - `main` - Main function template
  - `fn` - Function definition
  - `for`, `while`, `if` - Control flow structures
  - `vari`, `varf`, `vars` - Variable declarations
  - `import`, `from` - Import statements
  - And 20+ more!
- **File icons** for `.qa` files
- **Comment toggling** with `Ctrl+/` (supports `//`, `/* */`, and `#`)

### Quick Start

Create a new `.qa` file and try these snippets:

```quant
// Type "main" and press Tab
fn main() -> int {
    // Type "vari" and press Tab
    x: int = 42;
    return 0;
}
```

## Vim

Native Vim syntax highlighting with automatic file type detection.

### Installation

Copy the syntax files to your Vim config directory:

```bash
# Create directories if they don't exist
mkdir -p ~/.vim/syntax
mkdir -p ~/.vim/ftdetect

# Copy files
cp extension/vim/syntax/quant.vim ~/.vim/syntax/
cp extension/vim/ftdetect/quant.vim ~/.vim/ftdetect/
```

### Features

- **Syntax highlighting** for keywords, types, operators, and literals
- **Automatic detection** of `.qa` files
- **Comment support** for all three styles (`//`, `/* */`, `#`)
- **Number highlighting** (decimal, hex, octal, binary, float)

### Usage

Just open any `.qa` file - Vim will automatically apply Quant syntax highlighting:

```bash
vim src/main.qa
```

## Emacs / Doom Emacs

A complete Emacs major mode with syntax highlighting, indentation, and comment support.

### Installation (Doom Emacs)

Add to your `~/.doom.d/config.el`:

```elisp
;; Load Quant mode
(add-to-list 'load-path (expand-file-name "~/path/to/glados/extension/emacs"))
(require 'quant-mode)
```

Replace `~/path/to/glados` with your actual path.

Restart Emacs or run `M-x doom/reload`.

### Installation (Vanilla Emacs)

Add to your `~/.emacs` or `~/.emacs.d/init.el`:

```elisp
(add-to-list 'load-path (expand-file-name "~/path/to/glados/extension/emacs"))
(require 'quant-mode)
```

### Features

- **Syntax highlighting** with font-lock
- **Automatic indentation** (4 spaces per level)
- **Comment commands** (`M-;` to toggle comments)
- **Auto-mode** for `.qa` files

### Usage

Open a `.qa` file and Quant mode activates automatically. Use standard Emacs editing commands:

- `M-;` - Toggle comment on line/region
- `TAB` - Indent current line
- `C-M-\` - Indent region

## Neovim with Tree-sitter

For Neovim users with `nvim-treesitter`, we provide a Tree-sitter grammar for advanced features.

### Installation

1. Install the grammar:

```bash
cd extension/tree-sitter-quant
npm install
tree-sitter generate
```

2. Configure Neovim (`~/.config/nvim/init.lua` or `~/.config/nvim/lua/treesitter.lua`):

```lua
local parser_config = require("nvim-treesitter.parsers").get_parser_configs()

parser_config.quant = {
  install_info = {
    url = "~/path/to/glados/extension/tree-sitter-quant",
    files = {"src/parser.c"},
  },
  filetype = "qa",
}
```

3. Install and enable:

```vim
:TSInstall quant
:TSEnable highlight
```

### Features

- **Incremental parsing** - blazing fast updates as you type
- **Structural navigation** - jump between functions, blocks
- **Smart folding** - fold functions and blocks
- **Text objects** - select entire functions with `vif`

## Choosing an Editor

All editors provide excellent Quant support. Choose based on your preference:

| Editor | Best For | Setup Time |
|--------|----------|------------|
| **VSCode** | Modern IDE experience, snippets | 2 minutes |
| **Vim** | Fast, lightweight editing | 1 minute |
| **Emacs** | Powerful customization | 2 minutes |
| **Neovim** | Tree-sitter features | 5 minutes |

## Next Steps

- Set up the [code formatter](/tools/formatter) for consistent code style
- Explore [language basics](/guides/language-basics) to start coding
- Check out [code examples](/guides/getting-started)
