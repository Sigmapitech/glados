---
title: Code Formatter
description: Automatically format your Quant code with quant fmt
---

`quant fmt` is the official code formatter for Quant, inspired by `rustfmt`. It ensures consistent code style across your project.

## Installation

Build the formatter:

```bash
cd formatter
cabal build
```

Add to your PATH (optional):

```bash
# Add to ~/.bashrc or ~/.zshrc
export PATH="$PATH:$(cabal exec -- which quant-fmt)"
```

## Quick Start

Format your code (modifies files in-place):

```bash
quant fmt src/main.qa
quant fmt src/**/*.qa
```

Check if files are formatted (useful for CI):

```bash
quant fmt --check src/**/*.qa
```

## Usage

### Format Files (Default)

By default, `quant fmt` formats files **in-place**, just like `rustfmt`:

```bash
# Format a single file
quant fmt file.qa

# Format multiple files
quant fmt src/*.qa tests/*.qa

# Format all .qa files
quant fmt src/**/*.qa
```

Files are modified directly with consistent formatting applied.

### Check Mode (CI/CD)

Use `--check` to verify formatting without modifying files:

```bash
quant fmt --check src/**/*.qa
```

**Exit codes:**

- `0` - All files are formatted correctly
- `1` - Some files need formatting

Perfect for continuous integration:

```yaml
# .github/workflows/ci.yml
- name: Check code formatting
  run: quant fmt --check src/**/*.qa
```

### Print to Stdout

Use `--emit` to print formatted code instead of modifying files:

```bash
quant fmt --emit file.qa
quant fmt --emit file.qa | less
```

### Verbose Output

See which files were formatted:

```bash
quant fmt --verbose src/*.qa
# Output:
# ✓ src/main.qa
# ✓ src/utils.qa
#   src/config.qa (unchanged)
```

With `--check`:

```bash
quant fmt --check --verbose src/*.qa
# Output:
# ✓ src/main.qa
# ✗ src/utils.qa
# ✗ 1 file(s) need formatting
```

## Configuration

Create a `quantfmt.toml` file in your project root:

```toml
# Maximum line width
max_width = 100

# Indentation (spaces per level)
indent_size = 4

# Use hard tabs instead of spaces
hard_tabs = false

# Maximum array width before line-wrapping
array_width = 80

# Reorder imports alphabetically
reorder_imports = true

# Normalize comment formatting
normalize_comments = true
```

### Custom Config Path

```bash
quant fmt --config-path ./my-config.toml src/*.qa
```

### Config File Search

The formatter looks for config files in this order:

1. `--config-path` (if specified)
2. `quantfmt.toml` in current directory
3. `.quantfmt.toml` in current directory (hidden)
4. Default settings

## Formatting Style

### Functions

```quant
fn factorial(n: int) -> int {
    if n <= 1 {
        return 1;
    }
    return n * factorial(n - 1);
}
```

### Control Flow

```quant
if x > 0 {
    print("positive\n");
} elif x < 0 {
    print("negative\n");
} else {
    print("zero\n");
}
```

### Arrays

```quant
// Short arrays stay on one line
numbers: [int] = [1, 2, 3, 4, 5];

// Long arrays wrap
long_array: [int] = [
    1, 2, 3, 4, 5,
    6, 7, 8, 9, 10,
];
```

### Imports

```quant
import math;

from std.array import filter, map, reduce;
from std.io import print, println;
```

### Operators

- **Binary operators** - spaces around: `a + b`, `x == y`
- **Unary operators** - no space: `!flag`, `-x`, `~bits`
- **Assignments** - spaces around: `x = 10`, `y += 5`

## Integration

### Pre-commit Hook

Automatically check formatting before committing:

```bash
#!/bin/sh
# .git/hooks/pre-commit

FILES=$(git diff --cached --name-only | grep '\.qa$')
if [ -n "$FILES" ]; then
    quant fmt --check $FILES
    if [ $? -ne 0 ]; then
        echo "❌ Code formatting check failed."
        echo "Run: quant fmt $FILES"
        exit 1
    fi
fi
```

Make it executable:

```bash
chmod +x .git/hooks/pre-commit
```

### GitHub Actions

```yaml
name: CI

on: [push, pull_request]

jobs:
  format:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      
      - name: Setup Haskell
        uses: haskell/actions/setup@v2
        
      - name: Build formatter
        run: cabal build formatter
      
      - name: Check formatting
        run: cabal run formatter -- --check src/**/*.qa
```

### VSCode Integration

Add a task to `.vscode/tasks.json`:

```json
{
  "version": "2.0.0",
  "tasks": [
    {
      "label": "Format Quant",
      "type": "shell",
      "command": "quant fmt ${file}",
      "problemMatcher": [],
      "presentation": {
        "reveal": "never"
      }
    }
  ]
}
```

Then run with `Ctrl+Shift+P` → "Tasks: Run Task" → "Format Quant".

## Command Reference

```bash
quant fmt [OPTIONS] FILES...
```

### Options

| Option | Description |
|--------|-------------|
| `--check` | Check if files are formatted (exit 1 if not) |
| `--emit` | Print to stdout instead of modifying files |
| `--config-path PATH` | Use custom config file |
| `-v, --verbose` | Show detailed output |
| `-h, --help` | Show help message |

### Exit Codes

| Code | Meaning |
|------|---------|
| `0` | Success - all files formatted correctly |
| `1` | Files need formatting (in `--check` mode) or parse errors |

## Examples

### Format entire project

```bash
quant fmt src/**/*.qa tests/**/*.qa
```

### Check formatting in CI

```bash
quant fmt --check $(find . -name '*.qa')
```

### Format with custom config

```bash
quant fmt --config-path .quantfmt.toml src/**/*.qa
```

### Preview formatting

```bash
quant fmt --emit src/main.qa | less
```

### Format on save (watch mode)

```bash
# Using entr (install via package manager)
ls src/**/*.qa | entr quant fmt /_
```

## Comparison with rustfmt

| Feature | rustfmt | quant fmt |
|---------|---------|-----------|
| In-place by default | ✅ | ✅ |
| `--check` flag | ✅ | ✅ |
| Exit codes (0/1) | ✅ | ✅ |
| Config file | `rustfmt.toml` | `quantfmt.toml` |
| Print to stdout | `--emit stdout` | `--emit` |
| Verbose mode | `-v` | `-v` |

## Tips

1. **Format before committing** - Use the pre-commit hook to enforce formatting
2. **CI enforcement** - Fail builds on unformatted code with `--check`
3. **Team consistency** - Commit `quantfmt.toml` to your repository
4. **Editor integration** - Set up format-on-save for automatic formatting
5. **Gradual adoption** - Format files as you edit them, not all at once

## Troubleshooting

### Parse errors

If the formatter fails with parse errors, fix the syntax errors first:

```bash
quant fmt file.qa
# Error parsing file.qa:
# Unexpected token at line 10:5
```

### Config not found

The formatter warns but uses defaults if config is missing:

```bash
quant fmt --config-path missing.toml file.qa
# Warning: Config file not found: missing.toml
# Using default formatting options.
```

### Permission denied

Ensure you have write permissions for the files being formatted.

## Next Steps

- Learn about [editor support](/tools/editor-support) for real-time formatting
- Explore the [standard library](/reference/standard-library)
- Read about [language basics](/guides/language-basics)
