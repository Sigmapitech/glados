# Tree-sitter Grammar for Quant

A [Tree-sitter](https://tree-sitter.github.io/) grammar for the Quant programming language.

## What is Tree-sitter?

Tree-sitter is a parser generator tool and incremental parsing library. It provides:

- **Fast, incremental parsing** - Only re-parses changed code
- **Robust error recovery** - Highlights even syntactically invalid code
- **Universal adoption** - Used by GitHub, Neovim, Atom, Emacs, and more
- **Advanced features** - Enables syntax-aware code navigation, folding, and selection

## Features

This grammar provides:

- ✅ Complete Quant language syntax
- ✅ Syntax highlighting (via queries)
- ✅ Code folding support
- ✅ Smart indentation
- ✅ Scope tracking (functions, blocks)
- ✅ All operators and literals
- ✅ Import statements
- ✅ Comments (C-style `//`, `/* */`, Python-style `#`)

## Installation & Usage

### Prerequisites

```bash
npm install -g tree-sitter-cli
```

### Build the Parser

```bash
cd extension/tree-sitter-quant
npm install
tree-sitter generate
```

### Test the Parser

```bash
# Run tests
tree-sitter test

# Parse a file
tree-sitter parse ../../tests/array.qa

# Interactive playground
tree-sitter playground
```

### Use in Neovim

Add to your Neovim config:

```lua
-- ~/.config/nvim/init.lua or ~/.config/nvim/lua/treesitter.lua

require'nvim-treesitter.configs'.setup {
  highlight = {
    enable = true,
  },
}

-- Register Quant parser
local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
parser_config.quant = {
  install_info = {
    url = "~/path/to/glados/extension/tree-sitter-quant",
    files = {"src/parser.c"},
    branch = "main",
  },
  filetype = "qa",
}
```

Then install:

```vim
:TSInstall quant
```

### Use with GitHub

The grammar is automatically picked up by GitHub for `.qa` files once published to npm as `tree-sitter-quant`.

### Use in VSCode

VSCode doesn't use Tree-sitter natively yet, but you can use extensions like:
- [vscode-tree-sitter](https://marketplace.visualstudio.com/items?itemName=georgewfraser.vscode-tree-sitter)

## Development

### Project Structure

```
tree-sitter-quant/
├── grammar.js          # Grammar definition
├── package.json        # NPM package config
├── queries/            # Syntax queries
│   ├── highlights.scm  # Syntax highlighting
│   ├── locals.scm      # Scope tracking
│   ├── indents.scm     # Indentation rules
│   └── folds.scm       # Code folding
└── examples/           # Test examples
```

### Testing

Create test files in `test/corpus/`:

```
=====================================
Simple function
=====================================

fn add(a: int, b: int) -> int {
    return a + b;
}

-------------------------------------

(source_file
  (function_definition
    name: (identifier)
    parameters: (parameter_list
      (parameter name: (identifier) type: (primitive_type))
      (parameter name: (identifier) type: (primitive_type)))
    return_type: (primitive_type)
    body: (block
      (return_statement
        (binary_expression
          left: (identifier)
          operator: "+"
          right: (identifier))))))
```

Run tests with:

```bash
tree-sitter test
```

## Query System

Tree-sitter uses `.scm` files to define how to use the parse tree:

- **highlights.scm** - Syntax highlighting colors
- **locals.scm** - Variable scope tracking
- **indents.scm** - Smart indentation
- **folds.scm** - Code folding regions

## Publishing

To publish to npm (for GitHub and other tools):

```bash
npm login
npm publish
```

Once published, GitHub will automatically use it for `.qa` files!

## Resources

- [Tree-sitter Documentation](https://tree-sitter.github.io/)
- [Creating Parsers](https://tree-sitter.github.io/tree-sitter/creating-parsers)
- [Writing Queries](https://tree-sitter.github.io/tree-sitter/syntax-highlighting)

## License

BSD-2-Clause - See LICENSE file for details.
