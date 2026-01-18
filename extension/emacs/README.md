# Quant Mode for Emacs/Doom Emacs

Major mode for editing Quant programming language files in Emacs and Doom Emacs.

## Features

- Syntax highlighting for keywords, types, and constants
- Comment support (C-style `//`, `/* */`, and Python-style `#`)
- String and number literal highlighting
- Function name highlighting (definitions and calls)
- Automatic indentation
- Auto-detection of `.qa` files

## Installation

### Doom Emacs

1. Add to your `~/.doom.d/config.el`:

```elisp
;; ============================
;; Quant Mode (custom language)
;; ============================
(add-to-list 'load-path (expand-file-name "~/path/to/glados/extension/emacs"))
(require 'quant-mode)
```

**Important**: Replace `~/path/to/glados` with the actual path to your glados repository.

2. Restart Emacs or run:

```el
M-x doom/reload
```

3. Open any `.qa` file and verify the mode-line shows "Quant"

### Vanilla Emacs

1. Add to your `~/.emacs` or `~/.emacs.d/init.el`:

```elisp
(add-to-list 'load-path (expand-file-name "~/path/to/glados/extension/emacs"))
(require 'quant-mode)
```

**Important**: Replace `~/path/to/glados` with the actual path to your glados repository.

2. Restart Emacs

## Usage

Open any `.qa` file and Quant mode will be automatically activated.

### Manual Activation

If automatic detection doesn't work:

```el
M-x quant-mode
```

### Comments

Toggle comment on current line or region:

```el
M-;  (comment-dwim)
```

### Indentation

- `TAB` - Indent current line
- `C-M-\` - Indent region

## Customization

You can customize indentation width:

```elisp
(setq quant-indent-offset 4)  ; Default is 4 spaces
```

## Syntax Highlighting

The mode highlights:

- **Keywords**: `fn`, `return`, `if`, `else`, `for`, `while`, `continue`, `break`, `import`, `from`, `const`
- **Types**: `int`, `float`, `bool`, `str`, `void`
- **Constants**: `True`, `False`
- **Comments**: `//`, `/* */`, `#`
- **Numbers**: Decimal, hexadecimal (0x), octal (0o), binary (0b), and floating-point
- **Functions**: Function definitions and calls
- **Strings**: Single and double-quoted strings

## Example

```quant
# This is a Python-style comment
// This is a C-style comment

fn main() -> int {
    x: int = 42;
    y: float = 3.14;
    flag: bool = True;
    
    if x > 0 {
        print("Positive number\n");
    }
    
    return 0;
}
```

## Troubleshooting

If syntax highlighting doesn't work:

1. Check that the file has `.qa` extension
2. Manually activate with `M-x quant-mode`
3. Ensure `font-lock-mode` is enabled: `M-x font-lock-mode`

## Contributing

Feel free to report issues or submit pull requests to improve the mode.
