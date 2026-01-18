# Quant Code Formatter

Automatic code formatter for Quant programming language, inspired by `rustfmt`.

## Features

- **Opinionated formatting** - Consistent code style across projects  
- **In-place by default** - Formats files directly (like rustfmt)
- **Check mode** - Verify formatting in CI/CD
- **Configurable** - Use `quantfmt.toml` for custom settings
- **Fast** - Reuses existing parser, minimal overhead

## Installation

Build with Cabal:

```bash
cabal build formatter
```

## Quick Start

```bash
# Format files in-place (default, like rustfmt)
quant fmt src/*.qa

# Check if files are formatted (CI mode)
quant fmt --check src/*.qa

# Print to stdout instead
quant fmt --emit file.qa
```

## Usage

### Default: Format in-place (like rustfmt)

```bash
quant fmt file.qa
quant fmt src/**/*.qa
```

**Modifies files directly** - this is the default behavior.

### Check mode (for CI)

```bash
quant fmt --check file.qa
```

Returns exit code 1 if files need formatting. Perfect for CI/CD:

```yaml
# .github/workflows/ci.yml
- name: Check formatting
  run: quant fmt --check src/**/*.qa
```

### Emit to stdout

```bash
quant fmt --emit file.qa
```

Prints formatted code without modifying files.

### Verbose mode

```bash
quant fmt --verbose src/*.qa
quant fmt -v --check src/*.qa
```

Shows which files were formatted.

### Custom config

```bash
quant fmt --config-path ./quantfmt.toml file.qa
```

## Configuration

Create `quantfmt.toml` in your project root (like `rustfmt.toml`):

```toml
# Maximum line width
max_width = 100

# Indentation (spaces)
indent_size = 4

# Trailing commas
trailing_comma = "Vertical"

# Reorder imports
reorder_imports = true
```

See [quantfmt.toml](./quantfmt.toml) for all options.

## rustfmt Comparison

| Feature | rustfmt | quant fmt |
|---------|---------|-----------|
| In-place by default | ✅ | ✅ |
| `--check` with exit codes | ✅ | ✅ |
| Config file | `rustfmt.toml` | `quantfmt.toml` |
| Emit to stdout | `--emit stdout` | `--emit` |
| Verbose | `-v` | ✅ `-v` |

## Formatting Style

### Indentation

- Default: 4 spaces per level
- Configurable with `--indent`
- Always uses spaces (not tabs)

### Functions

```quant
fn add(x: int, y: int) -> int {
    return x + y;
}
```

### Blocks

```quant
if condition {
    statement1;
    statement2;
}
```

### Arrays

```quant
arr: [int] = [1, 2, 3, 4, 5];
```

### Imports

```quant
import math;
from std.io import print, println;
```

### Operators

- Spaces around binary operators: `a + b`
- No space for unary operators: `!flag`, `-x`
- Spaces around assignment: `x = 10`

## Integration

### Pre-commit Hook

Add to `.git/hooks/pre-commit`:

```bash
#!/bin/sh
quant fmt --check $(git diff --cached --name-only --diff-filter=ACM | grep '\.qa$')
```

### CI/CD

```yaml
# GitHub Actions
- name: Check formatting
  run: quant fmt --check **/*.qa
```

### VSCode

Add to `settings.json`:

```json
{
  "[quant]": {
    "editor.formatOnSave": true,
    "editor.defaultFormatter": "quant-team.quant"
  }
}
```

(Requires language server integration)

## Options

```zsh
Usage: quant fmt FILES... [--check] [--write] [--indent N]

Available options:
  -h,--help                Show this help text
  FILES...                 Quant source files to format
  -c,--check               Check if files are formatted without modifying them
  -w,--write               Write formatted output to files (in-place)
  -i,--indent N            Number of spaces per indentation level (default: 4)
```

## Examples

### Format single file

```bash
quant fmt src/main.qa
```

### Format multiple files

```bash
quant fmt src/*.qa tests/*.qa
```

### Format and save

```bash
quant fmt --write src/main.qa
```

### Check in CI

```bash
quant fmt --check $(find . -name '*.qa')
```

## Development

The formatter uses the existing Quant parser to build an AST, then pretty-prints it with consistent formatting rules.

### Architecture

```zsh
Input .qa file
  ↓
Parser (AST)
  ↓
Formatter (pretty-print)
  ↓
Formatted output
```

## License

BSD-2-Clause - See LICENSE file for details.
